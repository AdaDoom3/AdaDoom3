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
Name: 'SPM Exporter (.spm)...'
Blender: 270
Group: 'Export'
Tooltip: 'Export to space paritioned mesh file format (.spm)'
"""

__version__ = "1.0"
__bpydoc__ = """\
"""

bl_info = {
    "name": "SPM (Space paritioned mesh) Model Exporter",
    "description": "Exports a blender scene or object to the SPM format (the SuperTuxKart mesh format)",
    "version": (1,0),
    "blender": (2, 7, 0),
    "api": 31236,
    "location": "File > Export",
    "category": "Import-Export"}

import bpy, sys, os, os.path, struct, math, string, mathutils, bmesh

spm_parameters = {}
the_scene = None
spm_version = 1

# Axis conversion
axis_conversion = mathutils.Matrix([[1,0,0,0],[0,0,1,0],[0,1,0,0],[0,0,0,1]])

# Helper Functions
def writeFloat(value1):
    return struct.pack("<f", value1)

def writeInt(value1):
    return struct.pack("<i", value1)

def writeUint(value1):
    return struct.pack("<I", value1)

def writeInt16(value1):
    assert value1 > -32769
    assert value1 < 32768
    return struct.pack("<h", value1)

def writeUint16(value1):
    assert value1 < 65536
    return struct.pack("<H", value1)

def writeUint8(value1):
    assert value1 < 256
    return struct.pack("<B", value1)

def writeHalfFloat(float32):
    if sys.version_info[0] == 3 and sys.version_info[1] > 5:
        return struct.pack("<e", float32)
    else:
        import binascii
        F16_EXPONENT_BITS = 0x1F
        F16_EXPONENT_SHIFT = 10
        F16_EXPONENT_BIAS = 15
        F16_MANTISSA_BITS = 0x3ff
        F16_MANTISSA_SHIFT = (23 - F16_EXPONENT_SHIFT)
        F16_MAX_EXPONENT = (F16_EXPONENT_BITS << F16_EXPONENT_SHIFT)

        a = struct.pack('>f', float32)
        b = binascii.hexlify(a)

        f32 = int(b,16)
        f16 = 0
        sign = (f32 >> 16) & 0x8000
        exponent = ((f32 >> 23) & 0xff) - 127
        mantissa = f32 & 0x007fffff

        if exponent == 128:
            f16 = sign | F16_MAX_EXPONENT
            if mantissa:
                f16 |= (mantissa & F16_MANTISSA_BITS)
        elif exponent > 15:
            f16 = sign | F16_MAX_EXPONENT
        elif exponent > -15:
            exponent += F16_EXPONENT_BIAS
            mantissa >>= F16_MANTISSA_SHIFT
            f16 = sign | exponent << F16_EXPONENT_SHIFT | mantissa
        else:
            f16 = sign
        return writeUint16(f16)

def write2101010Rev(vector3):
    part = 0
    ret = 0
    v = min(1.0, max(-1.0, vector3[0]))
    if v > 0.0:
        part = (int)((v * 511.0) + 0.5)
    else:
        part = (int)((v * 512.0) - 0.5)
    ret |= part & 1023

    v = min(1.0, max(-1.0, vector3[1]))
    if v > 0.0:
        part = (int)((v * 511.0) + 0.5)
    else:
        part = (int)((v * 512.0) - 0.5)
    ret |= (part & 1023) << 10

    v = min(1.0, max(-1.0, vector3[2]))
    if v > 0.0:
        part = (int)((v * 511.0) + 0.5)
    else:
        part = (int)((v * 512.0) - 0.5)
    ret |= (part & 1023) << 20

    if len(vector3) == 4:
        v = min(1.0, max(-1.0, vector3[3]));
        if v > 0.0:
            part = (int)((v * 1.0) + 0.5);
        else:
            part = (int)((v * 2.0) - 0.5);
    else:
        part = 0
    ret |= (part & 3) << 30
    return writeUint(ret)

def writeLenString(value):
    encoded = str.encode(value)
    if len(encoded) > 255:
        value = encoded[0:255]
    bin = "<%ds" % len(encoded)
    tmp_buf = bytearray()
    tmp_buf += writeUint8(len(encoded))
    tmp_buf += struct.pack(bin, encoded)
    return tmp_buf

def writeMatrixAsLocRotScale(mat):
    loc, rot, scale = mat.decompose()
    rot.normalized()
    loc = loc.to_tuple()
    rot = (-rot.x, -rot.z, -rot.y, rot.w)
    scale = scale.to_tuple()
    return struct.pack('<ffffffffff', loc[0], loc[2], loc[1],\
    rot[0], rot[1], rot[2], rot[3], scale[0], scale[2], scale[1])

def getUniqueFrame(armature):
    unique_frame = []
    if armature.animation_data and armature.animation_data.action:
        ipo = armature.animation_data.action.fcurves
        for curve in ipo:
            if "pose" in curve.data_path:
                for keyframe in curve.keyframe_points:
                    if keyframe.co[0] < 0:
                        continue
                    global_key = int(keyframe.co[0])
                    global_key = 1 if global_key == 0 else global_key
                    if not global_key in unique_frame:
                        unique_frame.append(global_key)

    if armature.animation_data and armature.animation_data.nla_tracks:
        for nla_track in armature.animation_data.nla_tracks:
            for nla_strip in nla_track.strips:
                max_frame = int(nla_track.strips[-1].frame_end)
                if nla_strip.action:
                    for action_group in nla_strip.action.groups:
                        for curve in action_group.channels:
                            for keyframe in curve.keyframe_points:
                                if keyframe.co[0] < 0:
                                    continue
                                global_key = int(nla_strip.frame_start + keyframe.co[0])
                                if global_key > max_frame:
                                    global_key = int(nla_strip.frame_start)
                                global_key = 1 if global_key == 0 else global_key
                                #print('f: {} {} {}'.format(nla_strip.name, nla_strip.frame_start, keyframe.co[0]))
                                if not global_key in unique_frame:
                                    unique_frame.append(global_key)

    if armature.pose and armature.pose.bones:
        for pose_bone in armature.pose.bones:
            for constraint in pose_bone.constraints:
                try:
                    if constraint.target.animation_data.action:
                        ipo = constraint.target.animation_data.action.fcurves
                        for curve in ipo:
                            for modifier in curve.modifiers:
                                if modifier.frame_start > 0 and modifier.frame_end > 0:
                                    for f in range(int(modifier.frame_start), int(modifier.frame_end + 1)):
                                        #print('{} {}'.format(f, modifier.type))
                                        if not f in unique_frame:
                                            unique_frame.append(f)
                            #print('{}'.format(constraint.name))
                            for keyframe in curve.keyframe_points:
                                if keyframe.co[0] < 0:
                                    continue
                                global_key = int(keyframe.co[0])
                                global_key = 1 if global_key == 0 else global_key
                                #print('f: {} {}'.format(global_key, constraint.target.name))
                                if not global_key in unique_frame:
                                    #bpy.context.scene.frame_set(global_key)
                                    #bpy.context.scene.frame_current = global_key
                                    #armature.update_tag(refresh={'OBJECT', 'DATA'})
                                    #bpy.context.scene.update()
                                    #if constraint.influence == 0.0:
                                        #print('unused')
                                        #continue
                                    unique_frame.append(global_key)
                except (AttributeError) as e:
                    pass

    if len(unique_frame) == 0:
        print('No keyframes found for armature: {},'
        ' please remove the armature if it contains no keyframe.'.format(armature.name))
        return None
    unique_frame.sort()
    #for frame in unique_frame:
    #    print('unique_frame:{} {}'.format(frame, armature.name))
    if spm_parameters.get("keyframes-only") == False:
        first = bpy.context.scene.frame_start
        last = unique_frame[-1]
        unique_frame = []
        for frame in range(first, last + 1):
            unique_frame.append(frame)
        #for frame in unique_frame:
        #    print('unique_frame:{} {}'.format(frame, armature.name))
    return unique_frame

def equals(float1, float2):
    return (float1 + 0.0001 >= float2) and (float1 - 0.0001 <= float2)

class ExportArm:
    m_accumulated_bone = 0

    def __init__(self, arm):
        self.m_arm = arm
        self.m_bone_in_use = 0
        self.m_bone_local_id = []
        self.m_bone_names = {}
        for pose_bone in arm.pose.bones:
            self.m_bone_names[pose_bone.name] = 99999999

    def buildIndex(self, all_triangles):
        for triangle in all_triangles:
            if triangle.m_armature_name != self.m_arm.data.name:
                continue
            for i in range(0, 3):
                found = 0
                for joint_and_weight in triangle.m_all_joints_weights[i]:
                    if found > 3:
                        break
                    if joint_and_weight[0] in self.m_bone_names:
                        if self.m_bone_names[joint_and_weight[0]] == 99999999:
                            self.m_bone_names[joint_and_weight[0]] = ExportArm.m_accumulated_bone
                            triangle.m_all_joints[i][found] = ExportArm.m_accumulated_bone
                            triangle.m_all_weights[i][found] = joint_and_weight[1]
                            ExportArm.m_accumulated_bone += 1
                        else:
                            triangle.m_all_joints[i][found] = \
                            self.m_bone_names[joint_and_weight[0]]
                            triangle.m_all_weights[i][found] = joint_and_weight[1]
                        found += 1

    def buildLocalId(self):
        for k, v in self.m_bone_names.items():
            self.m_bone_local_id.append([k, v])
        self.m_bone_local_id.sort(key = lambda x: x[1])
        unused_bone = 0
        for bone_tu in self.m_bone_local_id:
            if bone_tu[1] != 99999999:
                bone_tu[1] = self.m_bone_in_use
                self.m_bone_in_use += 1
            else:
                bone_tu[1] = self.m_bone_in_use + unused_bone
                unused_bone += 1
        #print(self.m_bone_names)
        #print(unused_bone)
        #print(self.m_bone_in_use)
        #print(self.m_bone_local_id)

    def writeArmature(self):
            tmp_buf = bytearray()
            tmp_buf += writeUint16(self.m_bone_in_use)
            tmp_buf += writeUint16(len(self.m_arm.data.bones))

            assert len(self.m_bone_local_id) == len(self.m_arm.data.bones)
            for bone_tu in self.m_bone_local_id:
                bone = self.m_arm.data.bones[bone_tu[0]]
                tmp_buf += writeLenString(bone_tu[0])
            for bone_tu in self.m_bone_local_id:
                bone = self.m_arm.data.bones[bone_tu[0]]
                tmp_buf += writeMatrixAsLocRotScale(bone.matrix_local.inverted_safe())

            local_id_dict = {}
            for bone_pair in self.m_bone_local_id:
                local_id_dict[bone_pair[0]] = bone_pair[1]

            assert len(self.m_arm.pose.bones) == len(local_id_dict)
            for bone_tu in self.m_bone_local_id:
                pose_bone = self.m_arm.pose.bones[bone_tu[0]]
                if pose_bone.parent:
                    tmp_buf += writeInt16(local_id_dict[pose_bone.parent.name])
                else:
                    tmp_buf += writeInt16(-1)

            unique_frame = getUniqueFrame(self.m_arm)
            if unique_frame is None:
                return None
            
            tmp_buf += writeUint16(len(unique_frame))
            for frame in unique_frame:
                bpy.context.scene.frame_set(frame)
                tmp_buf += writeUint16(frame - 1)
                for bone_tu in self.m_bone_local_id:
                    pose_bone = self.m_arm.pose.bones[bone_tu[0]]
                    if pose_bone.parent:
                        bone_mat = pose_bone.parent.matrix.inverted_safe() * pose_bone.matrix
                    else:
                        if spm_parameters.get("local-space"):
                            bone_mat = pose_bone.matrix.copy()
                        else:
                            bone_mat = self.m_arm.matrix_world * pose_bone.matrix.copy()
                    tmp_buf += writeMatrixAsLocRotScale(bone_mat)
            return tmp_buf

class Vertex:
    m_cmp_joint = False

    def __init__(self):
        self.m_position = []
        self.m_normal = []
        self.m_color = []
        self.m_all_uvs = []
        self.m_tangent = []
        self.m_joints = []
        self.m_weights = []
        self.m_hash = 0

    def setHashString(self):
        # Round down floating point value
        self.m_hash = hash(str(round(self.m_position[0], 3)) +\
        str(round(self.m_position[1], 3)) + str(round(self.m_position[2], 3)) +\
        str(round(self.m_normal[0], 3)) + str(round(self.m_normal[1], 3)) +\
        str(round(self.m_normal[2], 3)) + str(round(self.m_all_uvs[0], 3)) +\
        str(round(self.m_all_uvs[3], 3)) + str(self.m_joints[0]) +\
        str(self.m_joints[1]) + str(round(self.m_weights[0], 3)) +\
        str(self.m_tangent[3])) if Vertex.m_cmp_joint\
        else hash(str(round(self.m_position[0], 3)) +\
        str(round(self.m_position[1], 3)) + str(round(self.m_position[2], 3)) +\
        str(round(self.m_normal[0], 3)) + str(round(self.m_normal[1], 3)) +\
        str(round(self.m_normal[2], 3)) + str(round(self.m_all_uvs[0], 3)) +\
        str(round(self.m_all_uvs[3], 3)) + str(self.m_tangent[3]))

    def __hash__(self):
        return self.m_hash

    def __eq__(self, other):
        return equals(self.m_position[0], other.m_position[0]) and\
        equals(self.m_position[1], other.m_position[1]) and\
        equals(self.m_position[2], other.m_position[2]) and\
        equals(self.m_normal[0], other.m_normal[0]) and\
        equals(self.m_normal[1], other.m_normal[1]) and\
        equals(self.m_normal[2], other.m_normal[2]) and\
        (self.m_color[0] == other.m_color[0]) and\
        (self.m_color[1] == other.m_color[1]) and\
        (self.m_color[2] == other.m_color[2]) and\
        equals(self.m_all_uvs[0], other.m_all_uvs[0]) and\
        equals(self.m_all_uvs[1], other.m_all_uvs[1]) and\
        equals(self.m_all_uvs[2], other.m_all_uvs[2]) and\
        equals(self.m_all_uvs[3], other.m_all_uvs[3]) and\
        (self.m_joints[0] == other.m_joints[0]) and\
        (self.m_joints[1] == other.m_joints[1]) and\
        (self.m_joints[2] == other.m_joints[2]) and\
        (self.m_joints[3] == other.m_joints[3]) and\
        equals(self.m_weights[0], other.m_weights[0]) and\
        equals(self.m_weights[1], other.m_weights[1]) and\
        equals(self.m_weights[2], other.m_weights[2]) and\
        equals(self.m_weights[3], other.m_weights[3]) and\
        self.m_tangent[3] == other.m_tangent[3] if Vertex.m_cmp_joint\
        else equals(self.m_position[0], other.m_position[0]) and\
        equals(self.m_position[1], other.m_position[1]) and\
        equals(self.m_position[2], other.m_position[2]) and\
        equals(self.m_normal[0], other.m_normal[0]) and\
        equals(self.m_normal[1], other.m_normal[1]) and\
        equals(self.m_normal[2], other.m_normal[2]) and\
        (self.m_color[0] == other.m_color[0]) and\
        (self.m_color[1] == other.m_color[1]) and\
        (self.m_color[2] == other.m_color[2]) and\
        equals(self.m_all_uvs[0], other.m_all_uvs[0]) and\
        equals(self.m_all_uvs[1], other.m_all_uvs[1]) and\
        equals(self.m_all_uvs[2], other.m_all_uvs[2]) and\
        equals(self.m_all_uvs[3], other.m_all_uvs[3]) and\
        self.m_tangent[3] == other.m_tangent[3]

    def writeVertex(self, uv_1, uv_2, vcolor, write_joints, need_export_tangent):
        tmp_buf = bytearray()
        for i in range(0, 3):
            tmp_buf += writeFloat(self.m_position[i])
        if spm_parameters.get("export-normal"):
            tmp_buf += write2101010Rev(self.m_normal)
        if vcolor:
            if self.m_color[0] == 255 and self.m_color[1] == 255 and\
            self.m_color[2] == 255:
                tmp_buf += writeUint8(128)
            else:
                tmp_buf += writeUint8(255)
                tmp_buf += writeUint8(self.m_color[0])
                tmp_buf += writeUint8(self.m_color[1])
                tmp_buf += writeUint8(self.m_color[2])
        if uv_1:
            tmp_buf += writeHalfFloat(self.m_all_uvs[0])
            tmp_buf += writeHalfFloat(self.m_all_uvs[1])
            if uv_2:
                tmp_buf += writeHalfFloat(self.m_all_uvs[2])
                tmp_buf += writeHalfFloat(self.m_all_uvs[3])
            if need_export_tangent:
                tmp_buf += write2101010Rev(self.m_tangent)
        if write_joints:
            tmp_buf += writeInt16(self.m_joints[0])
            tmp_buf += writeInt16(self.m_joints[1])
            tmp_buf += writeInt16(self.m_joints[2])
            tmp_buf += writeInt16(self.m_joints[3])
            tmp_buf += writeHalfFloat(self.m_weights[0])
            tmp_buf += writeHalfFloat(self.m_weights[1])
            tmp_buf += writeHalfFloat(self.m_weights[2])
            tmp_buf += writeHalfFloat(self.m_weights[3])
        return tmp_buf

class Triangle:
    def __init__(self):
        self.m_position = []
        self.m_normal = []
        self.m_color = []
        self.m_all_uvs = []
        self.m_tangent = []
        self.m_all_joints = [[-1, -1, -1, -1], [-1, -1, -1, -1],\
        [-1, -1, -1, -1]]
        self.m_all_weights = [[0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0],\
        [0.0, 0.0, 0.0, 0.0]]
        self.m_all_joints_weights = []
        self.m_texture_one = ""
        self.m_texture_two = ""
        self.m_texture_cmp = ""
        self.m_armature_name = ""
        self.m_hash = 0

    def get3Vertices(self):
        vertices = []
        for i in range(0, 3):
            vertices.append(Vertex())
            vertices[i].m_position = self.m_position[i]
            vertices[i].m_normal = self.m_normal[i]
            vertices[i].m_color = self.m_color[i]
            vertices[i].m_all_uvs = self.m_all_uvs[i]
            vertices[i].m_tangent = self.m_tangent[i]
            vertices[i].m_joints = self.m_all_joints[i]
            vertices[i].m_weights = self.m_all_weights[i]
            vertices[i].setHashString()
        return vertices

    def __hash__(self):
        return self.m_hash

    def __eq__(self, other):
        return self.m_position[0][0] == other.m_position[0][0] and\
        self.m_position[0][1] == other.m_position[0][1] and\
        self.m_position[0][2] == other.m_position[0][2] and\
        self.m_position[1][0] == other.m_position[1][0] and\
        self.m_position[1][1] == other.m_position[1][1] and\
        self.m_position[1][2] == other.m_position[1][2] and\
        self.m_position[2][0] == other.m_position[2][0] and\
        self.m_position[2][1] == other.m_position[2][1] and\
        self.m_position[2][2] == other.m_position[2][2]

    def setHashString(self):
        self.m_hash = hash(str(round(self.m_position[0][0], 7)) +\
        str(round(self.m_position[0][1], 7)) + str(round(self.m_position[0][2], 7)) +\
        str(round(self.m_position[1][0], 7)) + str(round(self.m_position[1][1], 7)) +\
        str(round(self.m_position[1][2], 7)) + str(round(self.m_position[2][0], 7)) +\
        str(round(self.m_position[2][1], 7)) + str(round(self.m_position[2][2], 7)))

# ==== Write SPM File ====
# (main exporter function)
def writeSPMFile(filename, objects=[]):

    bounding_boxes = [99999999.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    import time
    start = time.time()
    if objects:
        exp_obj = objects
    else:
        if spm_parameters.get("export-selected"):
            exp_obj = [ob for ob in bpy.data.objects if ob.select]
        else:
            exp_obj = bpy.data.objects

    has_vertex_color = False
    need_export_tangent = spm_parameters.get("export-tangent")
    arm_count = 0
    arm_dict = {}
    all_triangles = []
    static_mesh_frame = spm_parameters.get("static-mesh-frame")\
    if spm_parameters.get("static-mesh-frame") > 0 else bpy.context.scene.frame_start
    if static_mesh_frame < 1:
        print("static_mesh_frame is less than 1, changing it")
        static_mesh_frame = 1

    for obj in exp_obj:
        arm = obj.find_armature()
        if arm != None and not arm.data.name in arm_dict:
            arm_count += 1
            arm_dict[arm.data.name] = ExportArm(arm)

    if arm_count != 0:
        bpy.context.scene.frame_set(static_mesh_frame)

    all_no_uv_one = True
    for obj in exp_obj:
        tangents_triangles_dict = {}
        if obj.type != "MESH":
            continue

        if spm_parameters.get("local-space"):
            mesh_matrix = mathutils.Matrix()
        else:
            mesh_matrix = obj.matrix_world.copy()
        exported_matrix = axis_conversion * mesh_matrix

        arm = obj.find_armature()
        mesh = obj.to_mesh(the_scene, spm_parameters.get("apply-modifiers"), 'PREVIEW', False)
        if len(mesh.vertices) == 0:
            print('{} has no vertices, please check it'.format(obj.name))
            continue

        bm = bmesh.new()
        bm.from_mesh(mesh)
        bmesh.ops.transform(bm, matrix = exported_matrix, verts = bm.verts)
        if need_export_tangent:
            bmesh.ops.triangulate(bm, faces = bm.faces)
        # reverse the triangle winding for coordinate system in stk
        bmesh.ops.reverse_faces(bm, faces = bm.faces)
        bm.to_mesh(mesh)
        bm.free()

        mesh.calc_tessface()
        if len(mesh.tessfaces) == 0:
            print('{} has no faces, please check it'.format(obj.name))
            continue

        uv_one = True
        uv_two = True
        if (len(mesh.tessface_uv_textures) > 1):
            if (mesh.tessface_uv_textures.active is None):
                uv_one = False
                uv_two = False
        elif (len(mesh.tessface_uv_textures) > 0):
            if (mesh.tessface_uv_textures.active is None):
                uv_one = False
                uv_two = False
            else:
                uv_two = False
        else:
            uv_one = False
            uv_two = False

        # Smooth tangents ourselves
        if need_export_tangent:
            for poly in mesh.polygons:
                poly.use_smooth = False

        if uv_one and need_export_tangent:
            if all_no_uv_one:
                all_no_uv_one = False
            mesh.calc_tangents()
            for poly in mesh.polygons:
                # Because of triangulated
                assert(len(poly.loop_indices) == 3)
                poly_tri = Triangle()
                for li in poly.loop_indices:
                    poly_tri.m_position.append(mesh.vertices[mesh.loops[li].vertex_index].co)
                    loc_tan = mathutils.Vector(mesh.loops[li].tangent)
                    loc_tan.normalize()
                    poly_tri.m_tangent.append\
                    ((loc_tan[0], loc_tan[1], loc_tan[2], mesh.loops[li].bitangent_sign))
                poly_tri.setHashString()
                tangents_triangles_dict[poly_tri] = poly_tri.m_tangent

        for i, f in enumerate(mesh.tessfaces):
            texture_one = ""
            texture_two = ""
            if uv_one:
                if mesh.tessface_uv_textures[0].data[i].image != None:
                    texture_one = os.path.basename(bpy.path.abspath(mesh.tessface_uv_textures[0].data[i].image.filepath))
            if uv_two:
                if mesh.tessface_uv_textures[1].data[i].image != None:
                    texture_two = os.path.basename(bpy.path.abspath(mesh.tessface_uv_textures[1].data[i].image.filepath))
            texture_cmp = ''.join([texture_one, texture_two])
            vertex_list = []
            for j, v in enumerate(f.vertices):
                vertices = mesh.vertices[v].co
                if bounding_boxes[0] == 99999999.0:
                    bounding_boxes[0] = vertices[0]
                    bounding_boxes[1] = vertices[1]
                    bounding_boxes[2] = vertices[2]
                    bounding_boxes[3] = vertices[0]
                    bounding_boxes[4] = vertices[1]
                    bounding_boxes[5] = vertices[2]
                else:
                    # Min edge
                    if bounding_boxes[0] > vertices[0]:
                        bounding_boxes[0] = vertices[0]
                    if bounding_boxes[1] > vertices[1]:
                        bounding_boxes[1] = vertices[1]
                    if bounding_boxes[2] > vertices[2]:
                        bounding_boxes[2] = vertices[2]
                    # Max edge
                    if bounding_boxes[3] < vertices[0]:
                        bounding_boxes[3] = vertices[0]
                    if bounding_boxes[4] < vertices[1]:
                        bounding_boxes[4] = vertices[1]
                    if bounding_boxes[5] < vertices[2]:
                        bounding_boxes[5] = vertices[2]

                nor_vec = mathutils.Vector(mesh.vertices[v].normal)
                nor_vec.normalize()
                all_uvs = [0.0, 0.0, 0.0, 0.0]
                if uv_one:
                    all_uvs[0] = mesh.tessface_uv_textures[0].data[i].uv[j][0]
                    all_uvs[1] = 1 - mesh.tessface_uv_textures[0].data[i].uv[j][1]
                if uv_two:
                    all_uvs[2] = mesh.tessface_uv_textures[1].data[i].uv[j][0]
                    all_uvs[3] = 1 - mesh.tessface_uv_textures[1].data[i].uv[j][1]

                vertex_color = [255, 255, 255]
                if (len(mesh.tessface_vertex_colors) > 0):
                    if has_vertex_color == False:
                        has_vertex_color = True
                    if j == 0:
                        vcolor = mesh.tessface_vertex_colors[0].data[f.index].color1
                    elif j == 1:
                        vcolor = mesh.tessface_vertex_colors[0].data[f.index].color2
                    elif j == 2:
                        vcolor = mesh.tessface_vertex_colors[0].data[f.index].color3
                    elif j == 3:
                        vcolor = mesh.tessface_vertex_colors[0].data[f.index].color4
                    vertex_color = [min(int(vcolor.r * 255) , 255),\
                    min(int(vcolor.g * 255) , 255), min(int(vcolor.b * 255) , 255)]

                each_joint_data = []
                if arm_count != 0:
                    for group in mesh.vertices[v].groups:
                        each_joint_data.append((obj.vertex_groups[group.group].name, group.weight))
                    each_joint_data.sort(key = lambda x: x[1], reverse = True)
                vertex_list.append((vertices, nor_vec, vertex_color, all_uvs, each_joint_data))

            t1 = Triangle()
            # Because of triangulated
            if need_export_tangent:
                assert(len(vertex_list) == 3)
                for vertex in vertex_list:
                    t1.m_position.append(vertex[0])
                    t1.m_normal.append(vertex[1])
                    t1.m_color.append(vertex[2])
                    t1.m_all_uvs.append(vertex[3])
                    t1.m_all_joints_weights.append(vertex[4])
                t1.m_texture_one = texture_one
                t1.m_texture_two = texture_two
                t1.m_texture_cmp = texture_cmp
                t1.m_armature_name = arm.data.name if arm != None else "NULL"
                t1.setHashString()
                if t1 in tangents_triangles_dict:
                    t1.m_tangent = tangents_triangles_dict[t1]
                    #print("tangent:")
                    #print(t1.m_tangent)
                else:
                    if need_export_tangent and uv_one:
                        print("Missing a triangle from loop map")
                    t1.m_tangent = [(0.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0)]
                all_triangles.append(t1)
            else:
                for t in [0, 1, 2]:
                    t1.m_position.append(vertex_list[t][0])
                    t1.m_normal.append(vertex_list[t][1])
                    t1.m_color.append(vertex_list[t][2])
                    t1.m_all_uvs.append(vertex_list[t][3])
                    t1.m_all_joints_weights.append(vertex_list[t][4])
                t1.m_texture_one = texture_one
                t1.m_texture_two = texture_two
                t1.m_texture_cmp = texture_cmp
                t1.m_armature_name = arm.data.name if arm != None else "NULL"
                t1.setHashString()
                t1.m_tangent = [(0.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0)]
                all_triangles.append(t1)
                if (len(vertex_list) != 3):
                    t2 = Triangle()
                    for t in [0, 2, 3]:
                        t2.m_position.append(vertex_list[t][0])
                        t2.m_normal.append(vertex_list[t][1])
                        t2.m_color.append(vertex_list[t][2])
                        t2.m_all_uvs.append(vertex_list[t][3])
                        t2.m_all_joints_weights.append(vertex_list[t][4])
                    t2.m_texture_one = texture_one
                    t2.m_texture_two = texture_two
                    t2.m_texture_cmp = texture_cmp
                    t2.m_armature_name = arm.data.name if arm != None else "NULL"
                    t2.setHashString()
                    t2.m_tangent = [(0.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0), (0.0, 0.0, 0.0, 1.0)]
                    all_triangles.append(t2)

        if need_export_tangent: 
            mesh.free_tangents()
    if need_export_tangent and all_no_uv_one:
        print('{} (one of the object in the list) have no uvmap'.format(exp_obj[0].name))
        need_export_tangent = False

    if arm_count != 0:
        ExportArm.m_accumulated_bone = 0
        for arm_name in sorted(arm_dict.keys()):
            arm_dict[arm_name].buildIndex(all_triangles)
            arm_dict[arm_name].buildLocalId()

        useless_arm = True
        for triangle in all_triangles:
            for i in range(0, 3):
                total_weights = sum(triangle.m_all_weights[i])
                if total_weights > 0.0:
                    useless_arm = False
                    for j in range(0, 4):
                        triangle.m_all_weights[i][j] /= total_weights
        if useless_arm:
            arm_count = 0

    assert len(all_triangles) > 0
    all_triangles.sort(key = lambda x: x.m_texture_cmp)
    spm_buffer = bytearray()

    # SP header
    spm_buffer += writeUint16(20563)

    # 5 bit version, 3 bit type : SPMS SPMA SPMN
    # SPMS (space partitioned split mesh not supported in python)
    byte = 0
    byte = spm_version << 3
    byte |= 1 if arm_count != 0 else 2
    spm_buffer += writeUint8(byte)

    # bit 0: export-normal
    # bit 1: export-vcolor
    # bit 2: export-tangent
    byte = 0
    if spm_parameters.get("export-normal"):
        byte = 1
    export_vcolor = spm_parameters.get("export-vcolor") and has_vertex_color
    if export_vcolor:
        byte = 1 << 1 | byte
    if need_export_tangent:
        byte = 1 << 2 | byte
    spm_buffer += writeUint8(byte)
    for position in bounding_boxes:
        spm_buffer += writeFloat(position)

    tex_cmp = "NULL"
    texture_list = []
    for triangle in all_triangles:
        if triangle.m_texture_cmp != tex_cmp:
            tex_cmp = triangle.m_texture_cmp
            texture_list.append(triangle.m_texture_one)
            texture_list.append(triangle.m_texture_two)
    material_count = len(texture_list) >> 1
    spm_buffer += writeUint16(material_count)
    #print(material_count)
    for texture_name in texture_list:
        spm_buffer += writeLenString(texture_name)

    # No SPMS so always 1 sector count
    spm_buffer += writeUint16(1)

    vbo_ibo = bytearray()
    vertices_dict = {}
    vertices = []
    indices = []
    tex_cmp = all_triangles[0].m_texture_cmp
    material_count = 0
    mesh_buffer_count = 0
    Vertex.m_cmp_joint = arm_count != 0

    for t_idx in range(0, len(all_triangles) + 1):
        cur_cmp = all_triangles[t_idx].m_texture_cmp \
        if t_idx < len(all_triangles) else "NULL"
        if cur_cmp != tex_cmp or len(vertices) > 65532:
            tex_cmp = cur_cmp
            vbo_ibo += writeUint(len(vertices))
            vbo_ibo += writeUint(len(indices))
            vbo_ibo += writeUint16(material_count)
            #print(len(vertices))
            #print(len(indices))
            assert len(vertices) < 65536
            for vertex in vertices:
                if need_export_tangent:
                    tangent = mathutils.Vector((0.0, 0.0, 0.0))
                    bitangent_sign = vertices_dict.get(vertex)[1][0][3]
                    #print("All tangents accumlated:")
                    #print(vertices_dict.get(vertex)[1])
                    for each_tan in vertices_dict.get(vertex)[1]:
                        tangent = tangent +\
                        mathutils.Vector((each_tan[0], each_tan[1], each_tan[2]))
                    tangent.normalize()
                    vertex.m_tangent =\
                    (tangent[0], tangent[1], tangent[2], bitangent_sign)
                vbo_ibo += vertex.writeVertex(\
                all_triangles[t_idx -1].m_texture_one != "",\
                all_triangles[t_idx -1].m_texture_two != "",\
                export_vcolor, arm_count != 0, need_export_tangent)
            for index in indices:
                if len(vertices) > 255:
                    vbo_ibo += writeUint16(index)
                else:
                    vbo_ibo += writeUint8(index)
            if not len(vertices) > 65532:
                material_count = material_count + 1
            mesh_buffer_count += 1
            vertices_dict = {}
            vertices = []
            indices = []
        if t_idx >= len(all_triangles):
            break
        triangle = all_triangles[t_idx]
        assert len(triangle.m_position) == 3
        vertices_list = triangle.get3Vertices()
        for i in range(0, 3):
            vertex = vertices_list[i]
            if vertex not in vertices_dict:
                vertex_location = len(vertices)
                indices.append(vertex_location)
                vertices.append(vertex)
                vertices_dict[vertex] = [vertex_location, [vertex.m_tangent]]
            else:
                indices.append(vertices_dict[vertex][0])
                vertices_dict[vertex][1].append(vertex.m_tangent)

    spm_buffer += writeUint16(mesh_buffer_count)
    spm_buffer += vbo_ibo

    if arm_count != 0:
        spm_buffer += writeUint8(len(arm_dict))
        spm_buffer += writeUint16(static_mesh_frame - 1)
        for arm_name in sorted(arm_dict.keys()):
            armature = arm_dict[arm_name].writeArmature()
            if armature is not None:
                spm_buffer += armature

    spm = open(filename,'wb')
    spm.write(spm_buffer)
    spm.close()

    end = time.time()
    print("Exported in", (end - start))

# ==== CONFIRM OPERATOR ====
class SPM_Confirm_Operator(bpy.types.Operator):
    bl_idname = ("screen.spm_confirm")
    bl_label = ("File Exists, Overwrite?")

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        writeSPMFile(SPM_Confirm_Operator.filepath)
        return {'FINISHED'}

# ==== EXPORT OPERATOR ====

class SPM_Export_Operator(bpy.types.Operator):
    bl_idname = ("screen.spm_export")
    bl_label = ("SPM Export")

    filepath = bpy.props.StringProperty(subtype="FILE_PATH")
    selected = bpy.props.BoolProperty(name="Export selected only", default = False)
    localsp  = bpy.props.BoolProperty(name="Use local coordinates", default = False)
    applymodifiers = bpy.props.BoolProperty(name="Apply modifiers", default = True)
    do_sp = bpy.props.BoolProperty(name="Do mesh splitting (for space partitioning)", default = False)
    overwrite_without_asking = bpy.props.BoolProperty(name="Overwrite without asking", default = False)
    keyframes_only = bpy.props.BoolProperty(name="Export keyframes only for animated mesh", default = True)
    export_normal = bpy.props.BoolProperty(name="Export normal in mesh", default = True)
    export_vcolor = bpy.props.BoolProperty(name="Export vertex color in mesh", default = True)
    export_tangent = bpy.props.BoolProperty(name="Calculate tangent and bitangent sign for mesh", default = True)
    static_mesh_frame = bpy.props.IntProperty(name="Frame for static mesh usage", default = -1)

    def invoke(self, context, event):
        blend_filepath = context.blend_data.filepath
        if not blend_filepath:
            blend_filepath = "Untitled.spm"
        else:
            blend_filepath = os.path.splitext(blend_filepath)[0] + ".spm"
        self.filepath = blend_filepath

        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}

    def execute(self, context):

        global spm_parameters
        global the_scene
        spm_parameters["export-selected"] = self.selected
        spm_parameters["local-space"    ] = self.localsp
        spm_parameters["apply-modifiers"] = self.applymodifiers
        spm_parameters["keyframes-only"] = self.keyframes_only
        spm_parameters["export-normal"] = self.export_normal
        spm_parameters["export-vcolor"] = self.export_vcolor
        spm_parameters["export-tangent"] = self.export_tangent
        spm_parameters["static-mesh-frame"] = self.static_mesh_frame
        spm_parameters["do-sp"] = self.do_sp
        the_scene = context.scene

        if self.filepath == "":
            return {'FINISHED'}

        if not self.filepath.endswith(".spm"):
            self.filepath += ".spm"

        print("EXPORT", self.filepath)

        obj_list = []
        try:
            obj_list = context.scene.obj_list
        except:
            pass

        if len(obj_list) > 0:
            writeSPMFile(self.filepath, obj_list)
        else:
            if os.path.exists(self.filepath) and not self.overwrite_without_asking:
                SPM_Confirm_Operator.filepath = self.filepath
                bpy.ops.screen.spm_confirm('INVOKE_DEFAULT')
                return {'FINISHED'}
            else:
                writeSPMFile(self.filepath)
        return {'FINISHED'}

# Add to a menu
def menu_func_export(self, context):
    global the_scene
    the_scene = context.scene
    self.layout.operator(SPM_Export_Operator.bl_idname, text="SPM (.spm)")

def register():
    bpy.types.INFO_MT_file_export.append(menu_func_export)
    bpy.utils.register_module(__name__)

def unregister():
    bpy.types.INFO_MT_file_export.remove(menu_func_export)

if __name__ == "__main__":
    register
