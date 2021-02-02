#!BPY

# Copyright (c) 2017 SPM author(s)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Name: 'SPM Importer (.spm)...'
Blender: 270
Group: 'Import'
Tooltip: 'Import space paritioned mesh file format (.spm)'
"""

__version__ = "1.0"
__bpydoc__ = """\
"""

bl_info = {
    "name": "SPM (Space paritioned mesh) Model Importer",
    "description": "Imports SPM files (the SuperTuxKart mesh format)",
    "version": (1,0),
    "blender": (2, 7, 0),
    "api": 31236,
    "location": "File > Import",
    "category": "Import-Export"}

import bmesh, bpy, bpy_extras, os, os.path, struct, string, sys
from bpy_extras.image_utils import load_image
spm_version = 1

def reinterpretCastIntToFloat(int_val):
    return struct.unpack('f', struct.pack('I', int_val))[0]

def decompressHalfFloat(bytes):
    if sys.version_info[0] == 3 and sys.version_info[1] > 5:
        return struct.unpack("<e", bytes)[0]
    else:
        float16 = int(struct.unpack('<H', bytes)[0])
        # sign
        s = (float16 >> 15) & 0x00000001
        # exponent
        e = (float16 >> 10) & 0x0000001f
        # fraction
        f = float16 & 0x000003ff

        if e == 0:
            if f == 0:
                return reinterpretCastIntToFloat(int(s << 31))
            else:
                while not (f & 0x00000400):
                    f = f << 1
                    e -= 1
                e += 1
                f &= ~0x00000400
                #print(s,e,f)
        elif e == 31:
            if f == 0:
                return reinterpretCastIntToFloat(int((s << 31) | 0x7f800000))
            else:
                return reinterpretCastIntToFloat(int((s << 31) | 0x7f800000 |
                    (f << 13)))

        e = e + (127 -15)
        f = f << 13
        return reinterpretCastIntToFloat(int((s << 31) | (e << 23) | f))

def generateMeshBuffer(spm, vertices_count, indices_count,
                       read_normal, read_vcolor, read_tangent,
                       uv_one, uv_two, is_skinned, material_map,
                       material_id):
    obj_name =\
        (material_map[material_id][2] if material_map[material_id][2] else "_") +\
        "_" +\
        (material_map[material_id][3] if material_map[material_id][3] else "_")
    mesh = bpy.data.meshes.new(obj_name)
    obj = bpy.data.objects.new(obj_name, mesh)
    bm = bmesh.new()
    bm.from_mesh(mesh)

    vertices_list = []
    idx_size =\
        4 if vertices_count > 65535 else 2 if vertices_count > 255 else 1
    for vert in range(0, vertices_count):
        vc = None
        u = 0.0
        v = 0.0
        u_2 = 0.0
        v_2 = 0.0
        x, y, z = struct.unpack('<fff', spm.read(12))
        bm.verts.new((x, z, y))
        if read_normal:
            # Unused, auto re-calculate later
            spm.read(4)
        if read_vcolor:
            # Color identifier
            ci = struct.unpack('<B', spm.read(1))[0]
            if ci == 128:
                # All white
                vc = [1.0, 1.0, 1.0]
            else:
                r, g, b = struct.unpack('<BBB', spm.read(3))
                vc = [r / 255.0, g / 255.0, b / 255.0]
        if uv_one:
            u = decompressHalfFloat(spm.read(2))
            v = decompressHalfFloat(spm.read(2))
            v = 1.0 - v
            if uv_two:
                u_2 = decompressHalfFloat(spm.read(2))
                v_2 = decompressHalfFloat(spm.read(2))
                v_2 = 1.0 - v_2
            if read_tangent:
                # Unused
                spm.read(4)
        if is_skinned:
            # Unused
            spm.read(16)
        vertices_list.append((vc, (u, v, u_2, v_2)))
    indices_list = None
    if idx_size == 4:
        indices_list = struct.unpack("%dI" % (indices_count,),
            spm.read(indices_count * idx_size))
    elif idx_size == 2:
        indices_list = struct.unpack("%dH" % (indices_count,),
            spm.read(indices_count * idx_size))
    else:
        indices_list = struct.unpack("%dB" % (indices_count,),
            spm.read(indices_count * idx_size))

    # Required after adding / removing vertices and before accessing them
    # by index.
    bm.verts.ensure_lookup_table()
    # Required to actually retrieve the indices later on (or they stay -1).
    bm.verts.index_update()
    for i in range(0, len(indices_list), 3):
        try:
            bm.faces.new(bm.verts[j] for j in reversed(indices_list[i:i + 3]))
        except:
            # Face already exists
            continue

    if read_vcolor:
        color_layer = bm.loops.layers.color.new()
    if uv_one:
        uv_layer = bm.loops.layers.uv.new()
        tex_layer = bm.faces.layers.tex.new()
    if uv_two:
        uv_layer_two = bm.loops.layers.uv.new()
        tex_layer_two = bm.faces.layers.tex.new()

    for face in bm.faces:
        if uv_one:
            face[tex_layer].image = material_map[material_id][0]
        if uv_two:
            face[tex_layer_two].image = material_map[material_id][1]
        for loop in face.loops:
            if read_vcolor:
                loop[color_layer] = vertices_list[loop.vert.index][0]
            if uv_one:
                loop[uv_layer].uv = (vertices_list[loop.vert.index][1][0],
                    vertices_list[loop.vert.index][1][1])
            if uv_two:
                loop[uv_layer_two].uv = (vertices_list[loop.vert.index][1][2],
                    vertices_list[loop.vert.index][1][3])

    bmesh.ops.remove_doubles(bm, verts = bm.verts)
    bm.to_mesh(mesh)
    bm.free()

    for poly in mesh.polygons:
        poly.use_smooth = True

    bpy.context.scene.objects.link(obj)

def getImage(tex_name, working_directory, extra_tex_path):
    # Try in loaded images first:
    for image in bpy.data.images:
        if tex_name == os.path.basename(image.filepath):
            return image

    # Try local directory first
    img = bpy_extras.image_utils.load_image(tex_name, working_directory)
    if img is not None:
        return img
    img = bpy_extras.image_utils.load_image(tex_name, extra_tex_path,
        recursive = True)
    if img is not None:
        return img
    else:
        print("Missing image %s" % tex_name)
    return None

def loadSPM(context, filepath, extra_tex_path):
    spm = open(filepath, 'rb')

    sp_header = spm.read(2)
    if sp_header != b'SP':
        print('%s is not a valid spm file' % filepath)
        spm.close()
        return

    byte = struct.unpack('<B', spm.read(1))[0]
    version = byte >> 3;
    if version != spm_version:
        print('%d unsupported version' % version)
        spm.close()
        return

    byte &= ~0x08;
    header = None
    if byte == 0:
        header = "SPMS"
    elif byte == 1:
        header = "SPMA"
    else:
        header = "SPMN";

    byte = struct.unpack('<B', spm.read(1))[0]
    read_normal = byte & 0x01;
    read_vcolor = byte >> 1 & 0x01;
    read_tangent = byte >> 2 & 0x01;
    is_skinned = header == "SPMA";

    # Skip useless bounding box
    spm.read(24)
    material_count = struct.unpack('<H', spm.read(2))[0]
    material_map = []
    working_directory = os.path.dirname(filepath)
    for material in range(0, material_count):
        tex_name_1 = None
        tex_name_2 = None
        tex_fname_1 = None
        tex_fname_2 = None
        tex_size = struct.unpack('<B', spm.read(1))[0]
        if tex_size > 0:
            tex_name_1 = spm.read(tex_size).decode('ascii')
            tex_fname_1 = getImage(tex_name_1, working_directory,
                extra_tex_path);
        tex_size = struct.unpack('<B', spm.read(1))[0]
        if tex_size > 0:
            tex_name_2 = spm.read(tex_size).decode('ascii')
            tex_fname_2 = getImage(tex_name_2, working_directory,
                extra_tex_path);
        material_map.append((tex_fname_1, tex_fname_2, tex_name_1, tex_name_2))

    # Space partitioned mesh sector count, should be 1
    sector_count = struct.unpack('<H', spm.read(2))[0]

    for sector in range(0, sector_count):
        material_count = struct.unpack('<H', spm.read(2))[0]
        for material in range(0, material_count):
            vertices_count = struct.unpack('<I', spm.read(4))[0]
            indices_count = struct.unpack('<i', spm.read(4))[0]
            material_id = struct.unpack('<H', spm.read(2))[0]
            assert material_id < material_count
            generateMeshBuffer(spm, vertices_count, indices_count,
                read_normal, read_vcolor, read_tangent,
                material_map[material_id][2] is not None,
                material_map[material_id][3] is not None,
                is_skinned, material_map, material_id)
        if header == "SPMS":
            # Reserved, never used
            spm.read(24);

# ==== Import OPERATOR ====
from bpy_extras.io_utils import (ImportHelper)

class SPM_Import_Operator(bpy.types.Operator, ImportHelper):
    bl_idname = ("screen.spm_import")
    bl_label = ("SPM Import")
    filename_ext = ".spm"
    filter_glob = bpy.props.StringProperty(default="*.spm", options={'HIDDEN'})
    extra_tex_path = bpy.props.StringProperty(name="Texture path",\
    description="Extra directory for textures, importer will search recursively")

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "extra_tex_path")
    def execute(self, context):
        keywords = self.as_keywords(ignore=("filter_glob",))
        loadSPM(context, **keywords)
        context.scene.update()
        return {"FINISHED"}

# Add to a menu
def menu_func_import(self, context):
    self.layout.operator(SPM_Import_Operator.bl_idname, text="SPM (.spm)")

def register():
    bpy.types.INFO_MT_file_import.append(menu_func_import)
    bpy.utils.register_module(__name__)

def unregister():
    bpy.types.INFO_MT_file_import.remove(menu_func_import)

if __name__ == "__main__":
    register
