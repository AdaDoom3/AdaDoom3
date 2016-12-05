# ***** BEGIN GPL LICENSE BLOCK ***** 
# 
# This program is free software; you can redistribute it and/or 
# modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation; either version 2 
# of the License, or (at your option) any later version. 
# 
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details. 
# 
# You should have received a copy of the GNU General Public License 
# along with this program; if not, write to the Free Software Foundation, 
# Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
# 
# ***** END GPL LICENCE BLOCK *****

import os
import bpy


""" 
Name: 'ASCII Scene Exporter'
Blender: 2.53 Beta
Group: 'Export' 
Tooltip: 'ASCII Scene Export (*.ase)' 
""" 
__author__ = "Richard Bartlett" 
__version__ = "0.04"

#TODO:

aseFloat = lambda x: '''{0: 0.4f}'''.format(x)
scale = 1.0

class cHeader:
    def __init__(self):
        self.comment = "Blender 2.53 Ascii Scene Exporter for recent builds of blender"
        self.version = __version__

    def __repr__(self):
        return '''*3DSMAX_ASCIIEXPORT\t200\n*COMMENT "{0} v{1}"\n'''.format(self.comment, self.version)
        
class cScene:
    def __init__(self):
        self.filename = bpy.data.filepath
        self.firstframe = 0
        self.lastframe = 100
        self.framespeed = 30
        self.ticksperframe = 160
        self.backgroundstatic = ''.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.ambientstatic = ''.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        
    def __repr__(self):
        return '''*SCENE {{\n\t*SCENE_FILENAME "{0}"\n\t*SCENE_FIRSTFRAME {1}\n\t*SCENE_LASTFRAME {2}\n\t*SCENE_FRAMESPEED {3}\n\t*SCENE_TICKSPERFRAME {4}\n\t*SCENE_BACKGROUND_STATIC {5}\n\t*SCENE_AMBIENT_STATIC {6}\n}}\n'''.format(self.filename, self.firstframe, self.lastframe, self.framespeed, self.ticksperframe, self.backgroundstatic, self.ambientstatic)

class cMaterialList:
    def __init__(self):
        self.matlist = []
        if len(bpy.context.selected_objects) > 0:
            for object in bpy.context.selected_objects:
                for slot in object.material_slots:
                    mat = cMaterial(slot)
                    self.matlist.append(mat)
                    
    def dump(self):
        temp = ''
        for x in self.matlist:
            temp = temp + '\n\t*MATERIAL ' + str(self.matlist.index(x)) + ' {\n' + str(x) + '\n\t}'
        return temp
    
    def __repr__(self):
        return '''*MATERIAL_LIST {{\n\t*MATERIAL_COUNT {0}{1}\n}}'''.format(len(self.matlist), self.dump())
        
class cMaterial:
    def __init__(self, slot):
        self.name = slot.name
        self.matclass = 'Standard'
        self.ambient = ''.join([aseFloat(x) for x in [0.0, 0.0, 0.0]])
        self.diffuse = ''.join([aseFloat(x) for x in slot.material.diffuse_color])
        self.specular = ''.join([aseFloat(x) for x in slot.material.specular_color])
        self.shine = aseFloat(slot.material.specular_hardness / 511)
        self.shinestrength = aseFloat(slot.material.specular_intensity)
        self.transparency = aseFloat(slot.material.translucency * slot.material.alpha)
        self.wiresize = aseFloat(1.0)
        self.shading = str(slot.material.specular_shader).capitalize()
        self.xpfalloff = aseFloat(0.0)
        self.xptype = 'Filter'
        self.falloff = 'In'
        self.soften = False
        self.diffusemap = cDiffusemap(slot.material.texture_slots[0])
        self.submtls = []
        self.selfillum = aseFloat(slot.material.emit)
        
    def diffdump(self):
        for x in [self.diffusemap]:
            return x
    
    def __repr__(self):
        return '''\t\t*MATERIAL_NAME "{0}"\n\t\t*MATERIAL_CLASS "{1}"\n\t\t*MATERIAL_AMBIENT {2}\n\t\t*MATERIAL_DIFFUSE {3}\n\t\t*MATERIAL_SPECULAR {4}\n\t\t*MATERIAL_SHINE {5}\n\t\t*MATERIAL_SHINESTRENGTH {6}\n\t\t*MATERIAL_TRANSPARENCY {7}\n\t\t*MATERIAL_WIRESIZE {8}\n\t\t*MATERIAL_SHADING {9}\n\t\t*MATERIAL_XP_FALLOFF {10}\n\t\t*MATERIAL_SELFILLUM {11}\n\t\t*MATERIAL_FALLOFF {12}\n\t\t*MATERIAL_XP_TYPE {13}\n{14}'''.format(self.name, self.matclass, self.ambient, self.diffuse, self.specular, self.shine, self.shinestrength, self.transparency, self.wiresize, self.shading, self.xpfalloff, self.selfillum, self.falloff, self.xptype, self.diffdump())
        
class cDiffusemap:
    def __init__(self, slot):
        import os
    
        self.name = slot.name
        self.subno = 1
        self.amount = aseFloat(1.0)
        if slot.texture.type == 'IMAGE':
            self.mapclass = 'Bitmap'
            self.bitmap = slot.texture.image.filepath
            if slot.texture.image.has_data:
                pass
            else:
                self.bitmap = '\\\\base\\' + self.bitmap.replace('/','\\')
        else:
            self.mapclass = 'None'
            self.bitmap = 'None'
        self.type = 'Screen'
        self.uoffset = aseFloat(0.0)
        self.voffset = aseFloat(0.0)
        self.utiling = aseFloat(1.0)
        self.vtiling = aseFloat(1.0)
        self.angle = aseFloat(0.0)
        self.blur = aseFloat(1.0)
        self.bluroffset = aseFloat(0.0)
        self.noiseamt = aseFloat(1.0)
        self.noisesize = aseFloat(1.0)
        self.noiselevel = 1
        self.noisephase = aseFloat(0.0)
        self.bitmapfilter = 'Pyramidal'
        
    def __repr__(self):
        return '''\t\t*MAP_DIFFUSE {{\n\t\t\t*MAP_NAME "{0}"\n\t\t\t*MAP_CLASS "{1}"\n\t\t\t*MAP_SUBNO {2}\n\t\t\t*MAP_AMOUNT {3}\n\t\t\t*BITMAP "{4}"\n\t\t\t*MAP_TYPE {5}\n\t\t\t*UVW_U_OFFSET {6}\n\t\t\t*UVW_V_OFFSET {7}\n\t\t\t*UVW_U_TILING {8}\n\t\t\t*UVW_V_TILING {9}\n\t\t\t*UVW_ANGLE {10}\n\t\t\t*UVW_BLUR {11}\n\t\t\t*UVW_BLUR_OFFSET {12}\n\t\t\t*UVW_NOUSE_AMT {13}\n\t\t\t*UVW_NOISE_SIZE {14}\n\t\t\t*UVW_NOISE_LEVEL {15}\n\t\t\t*UVW_NOISE_PHASE {16}\n\t\t\t*BITMAP_FILTER {17}\n\t\t}}'''.format(self.name, self.mapclass, self.subno, self.amount, self.bitmap, self.type, self.uoffset, self.voffset, self.utiling, self.vtiling, self.angle, self.blur, self.bluroffset, self.noiseamt, self.noisesize, self.noiselevel, self.noisephase, self.bitmapfilter)

class cGeomObjList:
    def __init__(self):
        self.geolist = []
        if len(bpy.context.selected_objects) > 0:
            for object in bpy.context.selected_objects:
                if object.type == 'MESH':
                    geoobj = cGeomObject(object)
                    self.geolist.append(geoobj)
                    
    def dump(self):
        temp = ''
        for x in self.geolist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return self.dump()

class cGeomObject:
    def __init__(self, object):
        self.name = object.name
        self.prop_motionblur = 0
        self.prop_castshadow = 1
        self.prop_recvshadow = 1
        if object.material_slots[0].material.texture_slots[0].texture.type == 'IMAGE':  # I need to revisit this
            self.material_ref = bpy.context.selected_objects.index(object)
        else:
            self.material_ref = 0
        self.nodetm = cNodeTM(object)
        self.mesh = cMesh(object)
        
    def __repr__(self):
        return '''\n*GEOMOBJECT {{\n\t*NODE_NAME "{0}"\n{1}\n{2}\n\t*PROP_MOTIONBLUR {3}\n\t*PROP_CASTSHADOW {4}\n\t*PROP_RECVSHADOW {5}\n\t*MATERIAL_REF {6}\n}}'''.format(self.name, self.nodetm, self.mesh, self.prop_motionblur, self.prop_castshadow, self.prop_recvshadow, self.material_ref)

class cNodeTM:
    def __init__(self, object):
        self.name = object.name
        self.inherit_pos = '0 0 0'
        self.inherit_rot = '0 0 0'
        self.inherit_scl = '0 0 0'
        self.tm_row0 = '1.0000 0.0000 0.0000'
        self.tm_row1 = '0.0000 1.0000 0.0000'
        self.tm_row2 = '0.0000 0.0000 1.0000'
        self.tm_row3 = '0.0000 0.0000 0.0000'
        self.tm_pos = '0.0000 0.0000 0.0000'
        self.tm_rotaxis = '0.0000 0.0000 0.0000'
        self.tm_rotangle = '0.0000'
        self.tm_scale = '1.0000 1.0000 1.0000'
        self.tm_scaleaxis = '0.0000 0.0000 0.0000'
        self.tm_scaleaxisang = '0.0000'

    def __repr__(self):
        return '''\t*NODE_TM {{\n\t\t*NODE_NAME "{0}"\n\t\t*INHERIT_POS {1}\n\t\t*INHERIT_ROT {2}\n\t\t*INHERIT_SCL {3}\n\t\t*TM_ROW0 {4}\n\t\t*TM_ROW1 {5}\n\t\t*TM_ROW2 {6}\n\t\t*TM_ROW3 {7}\n\t\t*TM_POS {8}\n\t\t*TM_ROTAXIS {9}\n\t\t*TM_ROTANGLE {10}\n\t\t*TM_SCALE {11}\n\t\t*TM_SCALEAXIS {12}\n\t\t*TM_SCALEAXISANG {13}\n\t}}'''.format(self.name, self.inherit_pos, self.inherit_rot, self.inherit_scl, self.tm_row0, self.tm_row1, self.tm_row2, self.tm_row3, self.tm_pos, self.tm_rotaxis, self.tm_rotangle,  self.tm_scale, self.tm_scaleaxis, self.tm_scaleaxisang)
        
class cMesh:
    def __init__(self, object):
        self.timevalue = '0'
        self.numvertex = len(object.data.vertices)
        self.numfaces = len(object.data.faces)
        self.vertlist = cVertlist(object)
        self.facelist = cFacelist(object)
        self.tvertlist = cTVertlist(object)
        self.numtvertex = self.tvertlist.length
        self.numtvfaces = len(object.data.uv_texture_stencil.data)
        self.tfacelist = cTFacelist(self.numtvfaces)
        if len(object.data.vertex_colors) > 0:
            self.cvertlist = cCVertlist(object)
            self.numcvertex = self.cvertlist.length
            self.numcvfaces = len(object.data.vertex_colors[0].data)
            self.cfacelist = cCFacelist(self.numcvfaces)
            # change them into strings now
            self.cvertlist = '\n{0}'.format(self.cvertlist)
            self.numcvertex = '\n\t\t*MESH_NUMCVERTEX {0}'.format(self.numcvertex)
            self.numcvfaces = '\n\t\t*MESH_NUMCVFACES {0}'.format(self.numcvfaces)
            self.cfacelist = '\n{0}'.format(self.cfacelist)
        else:
            self.cvertlist = ''
            self.numcvertex = ''
            self.numcvfaces = ''
            self.cfacelist = ''
        self.normals = cNormallist(object)

    def __repr__(self):
        return '''\t*MESH {{\n\t\t*TIMEVALUE {0}\n\t\t*MESH_NUMVERTEX {1}\n\t\t*MESH_NUMFACES {2}\n{3}\n{4}\n\t\t*MESH_NUMTVERTEX {5}\n{6}\n\t\t*MESH_NUMTVFACES {7}\n{8}{9}{10}{11}{12}\n{13}\n\t}}'''.format(self.timevalue, self.numvertex, self.numfaces, self.vertlist, self.facelist, self.numtvertex, self.tvertlist, self.numtvfaces, self.tfacelist, self.numcvertex, self.cvertlist, self.numcvfaces, self.cfacelist, self.normals)

class cVertlist:
    def __init__(self, object):
        self.vertlist = []
        for data in object.data.vertices:
            temp = cVert(data.index, data.co.to_tuple(4))
            self.vertlist.append(temp)
            
    def dump(self):
        temp = ''
        for x in self.vertlist:
            temp = temp + str(x)
        return temp
            
    def __repr__(self):
        return '''\t\t*MESH_VERTEX_LIST {{\n{0}\t\t}}'''.format(self.dump())

class cVert:
    def __init__(self, index, coord):
        global scale
        self.index = index
        self.x = aseFloat(coord[0] * scale)
        self.y = aseFloat(coord[1] * scale)
        self.z = aseFloat(coord[2] * scale)
    
    def __repr__(self):
        return '''\t\t\t*MESH_VERTEX {0} {1} {2} {3}\n'''.format(self.index, self.x, self.y, self.z)

class cFacelist:
    def __init__(self, object):
        self.facelist = []
        for data in object.data.faces:
            temp = cFace(data.index, (data.vertices[0], data.vertices[1], data.vertices[2]))
            self.facelist.append(temp)

    def dump(self):
        temp = ''
        for x in self.facelist:
            temp = temp + str(x)
        return temp
        
    def __repr__(self):
        return '''\t\t*MESH_FACE_LIST {{\n{0}\t\t}}'''.format(self.dump())
        
class cFace:
    def __init__(self, index, vertices):
        self.index = index
        self.vertices = vertices
        
    def __repr__(self):
        return '''\t\t\t*MESH_FACE {0}: A: {1} B: {2} C: {3} AB: 0 BC: 0 CA: 0 *MESH_SMOOTHING 1 *MESH_MTLID 0\n'''.format(self.index, self.vertices[0], self.vertices[1], self.vertices[2])
        
class cTVertlist:
    def __init__(self, object):
        self.vertlist = []
        for index, face in enumerate(object.data.uv_texture_stencil.data):
            temp = cTVert((index * 3), face.uv1.to_tuple(4))
            self.vertlist.append(temp)
            temp = cTVert((index * 3)+1, face.uv2.to_tuple(4))
            self.vertlist.append(temp)
            temp = cTVert((index * 3)+2, face.uv3.to_tuple(4))
            self.vertlist.append(temp)
        self.length = len(self.vertlist)
            
    def dump(self):
        temp = ''
        for x in self.vertlist:
            temp = temp + str(x)
        return temp
            
    def __repr__(self):
        return '''\t\t*MESH_TVERTLIST {{\n{0}\t\t}}'''.format(self.dump())

class cTVert:
    def __init__(self, index, coord):
        self.index = index
        self.u = aseFloat(coord[0])
        self.v = aseFloat(coord[1])
    
    def __repr__(self):
        return '''\t\t\t*MESH_TVERT {0} {1} {2} 0.0000\n'''.format(self.index, self.u, self.v)

class cTFacelist:
    def __init__(self, facecount):
        self.facelist = []
        for data in range(facecount):
            temp = cTFace(data)
            self.facelist.append(temp)

    def dump(self):
        temp = ''
        for x in self.facelist:
            temp = temp + str(x)
        return temp
        
    def __repr__(self):
        return '''\t\t*MESH_TFACELIST {{\n{0}\t\t}}'''.format(self.dump())
        
class cTFace:
    def __init__(self, x):
        self.index = x
        self.vertices = []
        self.vertices.append(x*3)
        self.vertices.append((x*3)+1)
        self.vertices.append((x*3)+2)
        
    def __repr__(self):
        return '''\t\t\t*MESH_TFACE {0} {1} {2} {3}\n'''.format(self.index, self.vertices[0], self.vertices[1], self.vertices[2])

class cCVertlist:
    def __init__(self, object):
        temp = []
        if len(object.data.vertex_colors) > 0:
            for face in object.data.vertex_colors[0].data:
                temp.append(face.color1)
                temp.append(face.color2)
                temp.append(face.color3)
        self.vertlist = []
        for index, data in enumerate(temp):
            self.vertlist.append(cCVert(index, data))
        self.length = len(self.vertlist)
            
    def dump(self):
        temp = ''
        for x in self.vertlist:
            temp = temp + str(x)
        return temp
            
    def __repr__(self):
        return '''\t\t*MESH_CVERTLIST {{\n{0}\t\t}}'''.format(self.dump())

class cCVert:
    def __init__(self, index, data):
        self.index = index
        self.r = aseFloat(data[0])
        self.g = aseFloat(data[1])
        self.b = aseFloat(data[2])
    
    def __repr__(self):
        return '''\t\t\t*MESH_VERTCOL {0} {1} {2} {3}\n'''.format(self.index, self.r, self.g, self.b)

class cCFacelist:
    def __init__(self, facecount):
        temp = [0 for x in range(facecount)]
        self.facelist = []
        for index, data in enumerate(temp):
            self.facelist.append(cCFace(index, data))

    def dump(self):
        temp = ''
        for x in self.facelist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return '''\t\t*MESH_CFACELIST {{\n{0}\t\t}}'''.format(self.dump())

class cCFace:
    def __init__(self, index, data):
        self.index = index
        self.vertices = []
        self.vertices.append(index*3)
        self.vertices.append((index*3)+1)
        self.vertices.append((index*3)+2)
        
    def __repr__(self):
        return '''\t\t\t*MESH_CFACE {0} {1} {2} {3}\n'''.format(self.index, self.vertices[0], self.vertices[1], self.vertices[2])

class cNormallist:
    def __init__(self, object):
        self.normallist = []
        for face in object.data.faces:
            self.normallist.append(cNormal(face, object))

    def dump(self):
        temp = ''
        for x in self.normallist:
            temp = temp + str(x)
        return temp

    def __repr__(self):
        return '''\t\t*MESH_NORMALS {{\n{0}\t\t}}'''.format(self.dump())
            
class cNormal:
    def __init__(self, face, object):
        self.faceindex = face.index
        self.facenormal = [aseFloat(x) for x in face.normal.to_tuple(4)]
        self.vertnormals = []
        for x in face.vertices:
            self.vertnormals.append([x, [aseFloat(y) for y in object.data.vertices[x].normal.to_tuple(4)]])
          
    def __repr__(self):
        return '''\t\t\t*MESH_FACENORMAL {0} {1} {2} {3}\n\t\t\t\t*MESH_VERTEXNORMAL {4} {5} {6} {7}\n\t\t\t\t*MESH_VERTEXNORMAL {8} {9} {10} {11}\n\t\t\t\t*MESH_VERTEXNORMAL {12} {13} {14} {15}\n'''.format(self.faceindex, self.facenormal[0], self.facenormal[1], self.facenormal[2], self.vertnormals[0][0], self.vertnormals[0][1][0], self.vertnormals[0][1][1], self.vertnormals[0][1][2], self.vertnormals[1][0], self.vertnormals[1][1][0], self.vertnormals[1][1][1], self.vertnormals[1][1][2], self.vertnormals[2][0], self.vertnormals[2][1][0], self.vertnormals[2][1][1], self.vertnormals[2][1][2])
            
def gatherData():

    for object in bpy.context.selected_objects:
        if object.type != 'MESH':
            object.select = False
        else:
            # make current object active
            bpy.context.scene.objects.active = object
            
            # apply object transformations
            bpy.ops.object.location_apply()
            bpy.ops.object.scale_apply()
            bpy.ops.object.rotation_apply()
            
            # toggle edit/object mode back and forth to ensure mesh data is up to date
            bpy.ops.object.editmode_toggle()
            bpy.ops.object.editmode_toggle()
    
            # convert quads to tris
            if bpy.context.mode == 'OBJECT':
                bpy.ops.object.editmode_toggle()
            bpy.ops.mesh.select_all(action='SELECT')
            bpy.ops.mesh.quads_convert_to_tris()
            bpy.ops.object.editmode_toggle()
    
    header = cHeader()
    scene = cScene()
    matlist = cMaterialList()
    geolist = cGeomObjList()

    return '{0}{1}{2}{3}'.format(header,scene,matlist,geolist)

def exportASE(filename, data):
    print('Writing', filename)
    try:
        file = open(filename, 'w')
    except IOError:
        print('Error: The file could not be written to. Aborting.')
    else:
        file.write(data)
        file.close()
    print('Complete.')

from bpy.props import *

class EXPORT_OT_asel(bpy.types.Operator):
    '''Load an Ascii Scene Export File'''
    bl_idname = "export_scene.ase"
    bl_label = "Export ASE"
  
    filepath = StringProperty(name="File Path", description="File path used for exporting the ASE file", maxlen= 1024, default= "")
    ASE_SCALE = FloatProperty(name="Scale", description="Object scaling factor (default: 16)", min=0.01, max=1000.0, soft_min=0.01, soft_max=1000.0, default=16.0)
    
    def execute(self, context):
        import os
        global scale

        print('*** ASE Scene Exporter v' + __version__ + ' ***')
        
        if os.path.exists(self.properties.filepath):
            print(self.properties.filepath + ' already exists. Aborting.')
            return {'CANCELLED'}

        for object in bpy.context.selected_objects:
            if object.type == 'MESH':
                if len(object.material_slots) == 0:
                    print(object.name + ' has no material. Aborting.')
                    return {'CANCELLED'}
                if object.data.uv_texture_stencil_index == -1:
                    print(object.name + ' is not uv mapped. Aborting.')
                    return {'CANCELLED'}

        scale = self.properties.ASE_SCALE
        model = gatherData()
        exportASE(self.properties.filepath, model)
        return {'FINISHED'}
    
    def invoke(self, context, event):
        wm = context.window_manager
        # fixed for 2.56? Katsbits.com (via Nic B)
        # original wm.add_fileselect(self)
        wm.fileselect_add(self)
        return {'RUNNING_MODAL'}

menu_func = lambda self, context: self.layout.operator(EXPORT_OT_asel.bl_idname, text='Ascii Scene Export (.ase)')

def register():    
    bpy.types.INFO_MT_file_export.append(menu_func)
    
def unregister():
    bpy.types.INFO_MT_file_export.remove(menu_func)

if __name__ == '__main__':
    register()