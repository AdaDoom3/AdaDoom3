"""
Name: 'STK Material Exporter...'
Blender: 259
Group: 'Export'
Tooltip: 'Export a SuperTuxKart track scene'
"""
__author__ = ["Joerg Henrichs (hiker), Marianne Gagnon (Auria)"]
__url__ = ["supertuxkart.sourceforge.net"]
__version__ = "$Revision: 17088 $"
__bpydoc__ = """\
"""

bl_info = {
    "name": "SuperTuxKart Material Exporter",
    "description": "Exports image properties to the SuperTuxKart track format",
    "author": "Joerg Henrichs, Marianne Gagnon",
    "version": (1,0),
    "blender": (2, 5, 9),
    "api": 31236,
    "location": "File > Export",
    "warning": '', # used for warning icon and text in addons panel
    "wiki_url": "http://supertuxkart.sourceforge.net/Get_involved",
    "tracker_url": "https://sourceforge.net/apps/trac/supertuxkart/",
    "category": "Import-Export"}


def getScriptVersion():
    try:
        m = re.search('(\d+)', __version__)
        return str(m.group(0))
    except:
        return "Unknown"

import bpy
import bpy.path
import os

# ------------------------------------------------------------------------------
# Gets an id property of an object, returning the default if the id property
# is not set. If set_value_if_undefined is set and the property is not
# defined, this function will also set the property to this default value.
def getIdProperty(obj, name, default="", set_value_if_undefined=1):
    import traceback
    try:
        prop = obj[name]
        if isinstance(prop, str):
            return obj[name].replace('&', '&amp;') # this is XML
        else:
            return prop
    except:
        if default!=None and set_value_if_undefined:
            obj[name] = default
    return default

# --------------------------------------------------------------------------
# Write several ways of writing true/false as Y/N
def convertTextToYN(sText):
    sTemp = sText.strip().upper()
    if sTemp=="0" or sTemp[0]=="N" or sTemp=="FALSE":
        return "N"
    else:
        return "Y"

def merge_materials(x, y):
    z = x.copy()
    z.update(y)
    return z

# Writes the materials files, which includes all texture definitions 
# (remember: Blenders "image" objects are STK's "material" objects)
# Please use the STKProperty browser!!!
def writeMaterialsFile(sPath):
    
    # Work around the bug in blender where textures keep disappearing, by forcefully pinning all textures.
    for img in bpy.data.images:
        img.use_fake_user = True
    
    # Read & Write the materials to the file
    limage = bpy.data.images
    
    materfound = False
    for i in limage:
        for sAttrib in i.keys():
            materfound = True
            break
    if not materfound:
        print("No Materials defined.")
        return

    sp_mat_props = {
           'shader_name'            : {'default': "", 'parent': None, 'type': 'string'},
           'uv_two_tex'             : {'default': "", 'parent': None, 'type': 'string'},
           'tex_layer_2'            : {'default': "", 'parent': None, 'type': 'string'},
           'tex_layer_3'             : {'default': "", 'parent': None, 'type': 'string'},
           'tex_layer_4'            : {'default': "", 'parent': None, 'type': 'string'},
           'tex_layer_5'             : {'default': "", 'parent': None, 'type': 'string'}
    }

    old_mat_props = {
           'shader'                : {'default': 'solid', 'parent': None, 'type': 'string'},
           'splatting_texture_1'   : {'default': "", 'parent': ('shader','splatting'), 'type': 'string'},
           'splatting_texture_2'   : {'default': "", 'parent': ('shader','splatting'), 'type': 'string'},
           'splatting_texture_3'   : {'default': "", 'parent': ('shader','splatting'), 'type': 'string'},
           'splatting_texture_4'   : {'default': "", 'parent': ('shader','splatting'), 'type': 'string'},
           'normal_map'            : {'default': "", 'parent': None, 'type': 'string'},
           'gloss_map'             : {'default': "", 'parent': None, 'type': 'string'},
           'clampu'                : {'default': "N", 'parent': None, 'type': 'bool'},
           'clampv'                : {'default': "N", 'parent': None, 'type': 'bool'},
           'grass_speed'           : {'default': 0.4, 'parent': ('shader','grass'), 'type': 'number'},
           'grass_amplitude'       : {'default': 0.25, 'parent': ('shader','grass'), 'type': 'number'}
    }

    other_mat_props = {
           'below_surface'         : {'default': "N", 'parent': None, 'type': 'bool'},
           'collision_detect'      : {'default': "N", 'parent': None, 'type': 'bool'},
           'collision_particles'   : {'default': "", 'parent': 'collision_detect', 'type': 'string'},
           'collision_reaction'    : {'default': "none", 'parent': 'collision_detect', 'type': 'string'},
           'falling_effect'        : {'default': "N", 'parent': None, 'type': 'bool'},
           'ignore'                : {'default': "N", 'parent': None, 'type': 'bool'},
           'mask'                  : {'default': "", 'parent': None, 'type': 'string'},
           'mirror_axis'           : {'default': "none", 'parent': None, 'type': 'string'},
           'reset'                 : {'default': "N", 'parent': None, 'type': 'bool'},
           'surface'               : {'default': "N", 'parent': None, 'type': 'bool'},
           'high_adhesion'         : {'default': "N", 'parent': None, 'type': 'bool'},
           'has_gravity'           : {'default': "N", 'parent': None, 'type': 'bool'},
           'slowdown_time'         : {'default': 1.0, 'parent': 'use_slowdown', 'type': 'number'},
           'max_speed'             : {'default': 1.0, 'parent': 'use_slowdown', 'type': 'number'},
           'water_splash'          : {'default': "N", 'parent': None, 'type': 'bool'},
           'colorizable'           : {'default': "N", 'parent': None, 'type': 'bool'},
           'colorization_factor'   : {'default': 0.0, 'parent': 'colorizable', 'type': 'number'},
           'colorization_mask'     : {'default': "", 'parent': 'colorizable', 'type': 'string'},
           'hue_settings'          : {'default': "", 'parent': 'colorizable', 'type': 'string'}
    }

    #start_time = bsys.time()
    print("Writing material file --> \t")

    f = open(sPath+"/materials.xml", mode="w", encoding="utf-8")
    f.write("<?xml version=\"1.0\"?>\n")
    f.write("<!-- Generated with script from SVN rev %s -->\n"%getScriptVersion())
    f.write("<materials>\n")

    blendfile_dir = os.path.dirname(bpy.data.filepath)
    for i in limage:
    
        # Do not export materials from libraries
        if i.library is not None:
            continue
    
        # Only export materials from the same directory as the blend file
        abs_texture_path = bpy.path.abspath(i.filepath)
        if not bpy.path.is_subdir(abs_texture_path, blendfile_dir):
            continue

        #iterate through material definitions and collect data
        sImage = ""
        sSFX = ""
        sParticle = ""
        sZipper = ""
        hasSoundeffect = (convertTextToYN(getIdProperty(i, "use_sfx", "no")) == "Y")
        hasParticle = (convertTextToYN(getIdProperty(i, "particle", "no")) == "Y")
        hasZipper = (convertTextToYN(getIdProperty(i, "zipper", "no")) == "Y")

        # Create a copy of the list of defaults so that it can be modified. Then add
        # all properties of the current image
        l = []
        for sAttrib in i.keys():
            if sAttrib not in l:
                l.append( (sAttrib, i[sAttrib]) )

        #check if it's a sp material first
        sp_mat = getIdProperty(i, "shader", default="", set_value_if_undefined=0) == "sp_shader"
        if sp_mat == True:
            mat_dic = merge_materials(other_mat_props, sp_mat_props)
        else:
            mat_dic = merge_materials(other_mat_props, old_mat_props)

        for AProperty,ADefault in l:
            # Don't add the (default) values to the property list
            currentValue = getIdProperty(i, AProperty, ADefault, set_value_if_undefined=0)
            #Correct for all the ways booleans can be represented (true/false;yes/no;zero/not_zero) 
            if AProperty in mat_dic and mat_dic[AProperty]['type'] == 'bool':
                currentValue = convertTextToYN(currentValue)

            #These items pertain to the soundeffects (starting with sfx_)
            if AProperty.strip().startswith("sfx_"):
                strippedName = AProperty.strip()[len("sfx_"):]
                
                if strippedName in ['filename', 'rolloff', 'min_speed', 'max_speed', 'min_pitch', 'max_pitch', 'positional', 'volume']:
                    if isinstance(currentValue, float):
                        sSFX = "%s %s=\"%.2f\""%(sSFX,strippedName,currentValue)
                    else:
                        sSFX = "%s %s=\"%s\""%(sSFX,strippedName,currentValue)
            elif AProperty.strip().upper().startswith("PARTICLE_"):
                #These items pertain to the particles (starting with particle_)
                strippedName = AProperty.strip()[len("PARTICLE_"):]
                sParticle = "%s %s=\"%s\""%(sParticle,strippedName,currentValue)   
            elif AProperty.strip().upper().startswith("ZIPPER_"):
                #These items pertain to the zippers (starting with zipper_)
                strippedName = AProperty.strip()[len("ZIPPER_"):]
                
                sZipper = "%s %s=\"%s\""%(sZipper,strippedName.replace('_', '-'),currentValue)   
            else:
                #These items are standard items
                prop = AProperty.strip()#.lower()
                
                if prop in mat_dic.keys():
                    
                    # if this property is conditional on another
                    cond = mat_dic[prop]['parent']
                    
                    conditionPassed = False
                    if cond is None:
                        conditionPassed = True
                    elif type(cond) is tuple:
                        if cond[0] in i and i[cond[0]] == cond[1]:
                            conditionPassed = True
                    elif cond in i and i[cond] == "true":
                        conditionPassed = True
                        
                    
                    if currentValue != mat_dic[prop]['default'] and conditionPassed:
                        fixed_property = AProperty
                        if AProperty == 'shader_name':
                            fixed_property = 'shader'
                        if isinstance(currentValue, float):
                            # In blender, proeprties use '_', but STK still expects '-'
                            sImage = "%s %s=\"%.2f\""%(sImage,fixed_property.replace("_","-"),currentValue)
                        else:
                            # In blender, proeprties use '_', but STK still expects '-'
                            sImage = "%s %s=\"%s\""%(sImage,fixed_property.replace("_","-"),(currentValue+'').strip())

        # Now write the main content of the materials.xml file
        if sImage or hasSoundeffect or hasParticle or hasZipper:
            #Get the filename of the image.
            s = i.filepath
            sImage="  <material name=\"%s\"%s" % (bpy.path.basename(s),sImage)                
            if hasSoundeffect:
                sImage="%s>\n    <sfx%s/" % (sImage,sSFX)
            if hasParticle:
                sImage="%s>\n    <particles%s/" % (sImage,sParticle)
            if hasZipper:
                sImage="%s>\n    <zipper%s/" % (sImage,sZipper)
            if not hasSoundeffect and not hasParticle and not hasZipper:
                sImage="%s/>\n" % (sImage)
            else:
                sImage="%s>\n  </material>\n" % (sImage)
      
            f.write(sImage)
        
    f.write("</materials>\n")

    f.close()
    #print bsys.time()-start_time,"seconds"
    # ----------------------------------------------------------------------

class STK_Material_Export_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_material_exporter")
    bl_label = ("Export Materials")
    filepath = bpy.props.StringProperty()

    def execute(self, context):
        writeMaterialsFile(self.filepath)
        return {'FINISHED'}

def register():
    bpy.utils.register_module(__name__)
    
