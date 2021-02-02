#!BPY

#(setq tab-width 4)
#(setq py-indent-offset `4)

"""
Name: 'STK Kart Exporter (.irrkart)...'
Blender: 259
Group: 'Export'
Tooltip: 'Export a SuperTuxKart kart'
"""
__author__  = ["Joerg Henrichs (hiker), Marianne Gagnon (Auria), xapantu"]
__url__     = ["supertuxkart.sourceforge.net"]
__version__ = "$Revision: 16945 $"
__bpydoc__  = """\
"""

# Copyright (C) 2009-2011 Joerg Henrichs, Marianne Gagnon, Xapantu

bl_info = {
    "name": "SuperTuxKart Kart Exporter",
    "description": "Exports a blender character/kart to SuperTuxKart",
    "author": "Joerg Henrichs, Marianne Gagnon, Xapantu",
    "version": (3,0),
    "blender": (2, 5, 9),
    "api": 31236,
    "location": "File > Export",
    "warning": '', # used for warning icon and text in addons panel
    "wiki_url": "http://supertuxkart.sourceforge.net/Get_involved",
    "tracker_url": "https://sourceforge.net/apps/trac/supertuxkart/",
    "category": "Import-Export"}

#If you get an error here, it might be
#because you don't have Python installed.
import bpy
import sys,os,os.path,struct,math,string,re

from mathutils import *

operator = None
the_scene = None

log = []

thelist = []
def getlist(self):
    global thelist
    return thelist
def setlist(self, value):
    global thelist
    thelist = value

def log_info(msg):
    print("INFO:", msg)
    log.append( ('INFO', msg) )
def log_warning(msg):
    print("WARNING:", msg)
    log.append( ('WARNING', msg) )
def log_error(msg):
    print("ERROR:", msg)
    log.append( ('ERROR', msg) )
    
    
# ------------------------------------------------------------------------------
# Returns a game logic property
def getProperty(obj, name, default=""):
    try:
        return obj[name]
    except:
        return default

# ------------------------------------------------------------------------------
# Returns the version of this script
def getScriptVersion():
    m = re.search('(\d+)', __version__)
    if m:
            return str(m.group(0))
    return "0.1"

# ------------------------------------------------------------------------------
# Save nitro emitter
def saveNitroEmitter(f, lNitroEmitter, path):
    if len(lNitroEmitter) != 2:
        log_warning("Warning - %d nitro emitter specified. Only 2 are allowed" % len(lNitroEmitter))
        return
    
    f.write('  <nitro-emitter>\n')
    f.write('    <nitro-emitter-a position = "%f %f %f" />\n' \
                % (lNitroEmitter[0].location.x, lNitroEmitter[0].location.z, lNitroEmitter[0].location.y))
    f.write('    <nitro-emitter-b position = "%f %f %f" />\n' \
                % (lNitroEmitter[1].location.x, lNitroEmitter[1].location.z, lNitroEmitter[1].location.y))
    f.write('  </nitro-emitter>\n')
    
# ------------------------------------------------------------------------------

def saveHeadlights(f, lHeadlights, path, straight_frame):
    if len(lHeadlights) == 0:
        return
    if 'spm_export' not in dir(bpy.ops.screen):
        log_error("Cannot find the spm exporter, make sure you installed it properly")
        return
    
    f.write('  <headlights>\n')
    instancing_objects = {}
    for obj in lHeadlights:
        bone_name = None
        if obj.parent and obj.parent_type == 'BONE':
            if straight_frame == -1:
                print("Missing striaght frame for saving straight location")
                assert False
            bone_name = obj.parent_bone
            bpy.context.scene.frame_set(straight_frame)
        loc, rot, scale = obj.matrix_world.decompose()
        rot = rot.to_euler('XZY')
        rad2deg = -180.0 / 3.1415926535;
        flags = []
        flags.append('    <object position="%f %f %f"\n' % (loc[0], loc[2], loc[1]))
        flags.append('           rotation="%f %f %f"\n' % (rot[0] * rad2deg, rot[2] * rad2deg, rot[1] * rad2deg))
        flags.append('           scale="%f %f %f"\n' % (scale[0], scale[2], scale[1]))
        if bone_name:
            flags.append('           bone="%s"\n' % bone_name)
        headlight_color = getProperty(obj, 'headlight_color', '255 255 255')
        if headlight_color != '255 255 255':
            flags.append('           color=\"%s\"\n' % headlight_color)

        exported_name = obj.name
        if obj.data.name in instancing_objects:
            exported_name = instancing_objects[obj.data.name]
        else:
            instancing_objects[obj.data.name] = obj.name
            global the_scene
            the_scene.obj_list = [obj]
            bpy.ops.screen.spm_export(localsp=True, filepath=path + "/" + obj.name,
                                      export_tangent='precalculate_tangents' in bpy.context.scene\
                                      and bpy.context.scene['precalculate_tangents'] == 'true',
                                      overwrite_without_asking=True)
            the_scene.obj_list = []

        flags.append('           model="%s.spm"/>\n' % exported_name)
        f.write('%s' % ' '.join(flags))
    f.write('  </headlights>\n')

# ------------------------------------------------------------------------------
# Save speed weighted
def saveSpeedWeighted(f, lSpeedWeighted, path, straight_frame):
    if len(lSpeedWeighted) == 0:
        return
    if 'spm_export' not in dir(bpy.ops.screen):
        log_error("Cannot find the spm exporter, make sure you installed it properly")
        return

    f.write('  <speed-weighted-objects>\n')
    instancing_objects = {}
    for obj in lSpeedWeighted:
        bone_name = None
        if obj.parent and obj.parent_type == 'BONE':
            if straight_frame == -1:
                print("Missing striaght frame for saving straight location")
                assert False
            bone_name = obj.parent_bone
            bpy.context.scene.frame_set(straight_frame)
        loc, rot, scale = obj.matrix_world.decompose()
        rot = rot.to_euler('XZY')
        rad2deg = -180.0 / 3.1415926535;
        flags = []
        flags.append('    <object position="%f %f %f"\n' % (loc[0], loc[2], loc[1]))
        flags.append('           rotation="%f %f %f"\n' % (rot[0] * rad2deg, rot[2] * rad2deg, rot[1] * rad2deg))
        flags.append('           scale="%f %f %f"\n' % (scale[0], scale[2], scale[1]))
        if bone_name:
            flags.append('           bone="%s"\n' % bone_name)

        strength_factor = float(getProperty(obj, "speed-weighted-strength-factor", -1.0))
        speed_factor    = float(getProperty(obj, "speed-weighted-speed-factor",    -1.0))
        texture_speed_x = float(getProperty(obj, "speed-weighted-texture-speed-x", 0.0))
        texture_speed_y = float(getProperty(obj, "speed-weighted-texture-speed-y", 0.0))

        attr = ""
        if strength_factor >= 0.0:
            attr = attr + ' strength-factor="%f"' % strength_factor
        if speed_factor >= 0.0:
            attr = attr + ' speed-factor="%f"' % speed_factor
        if texture_speed_x != 0.0 or texture_speed_y != 0.0:
            attr = attr + ' texture-speed-x="%f" texture-speed-y="%f"' % (texture_speed_x, texture_speed_y)
        flags.append('          %s\n' % attr)

        exported_name = obj.name
        if obj.data.name in instancing_objects:
            exported_name = instancing_objects[obj.data.name]
        else:
            instancing_objects[obj.data.name] = obj.name
            global the_scene
            the_scene.obj_list = [obj]
            bpy.ops.screen.spm_export(localsp=True, filepath=path + "/" + obj.name,
                                      export_tangent='precalculate_tangents' in bpy.context.scene\
                                      and bpy.context.scene['precalculate_tangents'] == 'true',
                                      overwrite_without_asking=True)
            the_scene.obj_list = []

        flags.append('           model="%s.spm"/>\n' % exported_name)
        f.write('%s' % ' '.join(flags))
    f.write('  </speed-weighted-objects>\n')

# ------------------------------------------------------------------------------
def saveWheels(f, lWheels, path):
    if len(lWheels) == 0:
        return
    if 'spm_export' not in dir(bpy.ops.screen):
        log_error("Cannot find the spm exporter, make sure you installed it properly")
        return
        
    if len(lWheels)!=4:
        log_warning("Warning - %d wheels specified" % len(lWheels))

    lWheelNames = ("wheel-front-right.spm", "wheel-front-left.spm",
                   "wheel-rear-right.spm",  "wheel-rear-left.spm"   )
    lSides      = ('front-right', 'front-left', 'rear-right', 'rear-left')

    f.write('  <wheels>\n')
    for wheel in lWheels:
        name = wheel.name.upper()
        # If old stylen names are given, use them to determine
        # which wheel is which.
        #if name=="WHEELFRONT.R":
        #    index=0
        #elif name=="WHEELFRONT.L":
        #    index=1
        #elif name=="WHEELREAR.R":
        #    index=2
        #elif name=="WHEELREAR.L":
        #    index=3
        #else:
        
        # Otherwise the new style 'type=wheel' is used. Use the x and
        #  y coordinates to determine where the wheel belongs to.
        x = wheel.location.x
        y = wheel.location.y
        index = 0
        if y<0:
            index=index+2
        if x<0: index=index+1
        
        f.write('    <%s position = "%f %f %f"\n' \
                % ( lSides[index], wheel.location.x, wheel.location.z, wheel.location.y))
        f.write('                 model    = "%s"       />\n'%lWheelNames[index])
        lOldPos = Vector([wheel.location.x, wheel.location.y, wheel.location.z])
        wheel.location = Vector([0, 0, 0])
        
        global the_scene
        the_scene.obj_list = [wheel]
        
        bpy.ops.screen.spm_export(localsp=False, filepath=path + "/" + lWheelNames[index],
                                  export_tangent='precalculate_tangents' in bpy.context.scene\
                                  and bpy.context.scene['precalculate_tangents'] == 'true',
                                  overwrite_without_asking=True)
        the_scene.obj_list = []
        
    
        wheel.location = lOldPos
                                  
    f.write('  </wheels>\n')

# ------------------------------------------------------------------------------
# Saves any defined animations to the kart.xml file.
def saveAnimations(f):
    global the_scene
    first_frame = the_scene.frame_start
    last_frame  = the_scene.frame_end
    straight_frame = -1
    # search for animation
    lAnims = []
    lMarkersFound = []
    for i in range(first_frame, last_frame+1):

        # Find markers at this frame
        for curr in the_scene.timeline_markers:
            if curr.frame == i:
                markerName = curr.name.lower()
                if  markerName in \
                   ["straight", "right", "left", "start-winning", "start-winning-loop",
                    "end-winning", "start-losing", "start-losing-loop", "end-losing",
                    "start-explosion", "end-explosion", "start-jump", "start-jump-loop", "end-jump",
                    "turning-l", "center", "turning-r", "repeat-losing", "repeat-winning",
                    "backpedal-left", "backpedal", "backpedal-right", "selection-start", "selection-end"]:
                    if markerName=="turning-l": markerName="left"
                    if markerName=="turning-r": markerName="right"
                    if markerName=="center": markerName="straight"
                    if markerName=="straight" : straight_frame = i
                    if markerName=="repeat-losing": markerName="start-losing-loop"
                    if markerName=="repeat-winning": markerName="start-winning-loop"
                    lAnims.append( (markerName, i-1) )
                    lMarkersFound.append(markerName)

    if (not "straight" in lMarkersFound) or (not "left" in lMarkersFound) or (not "right" in lMarkersFound):
        log_warning('Could not find markers left/straight/right in frames %i to %i, steering animations may not work' %  (first_frame, last_frame))
	
    if (not "start-winning" in lMarkersFound) or (not "start-losing" in lMarkersFound) or (not "end-winning" in lMarkersFound) or (not "end-losing" in lMarkersFound):
        log_warning('Could not find markers for win/lose animations in frames %i to %i, win/lose animations may not work' %  (first_frame, last_frame))
        
    
    if lAnims:
        f.write('  <animations %s = "%s"' % (lAnims[0][0], lAnims[0][1]))
        for (marker, frame) in lAnims[1:]:
                f.write('\n              %s = "%s"'%(marker, frame))
        f.write('/>\n')
    return straight_frame
    
# ------------------------------------------------------------------------------
# Code for saving kart specific sounds. This is not yet supported, but for
# now I'll leave the code in plase
def saveSounds(f, engine_sfx):
    lSounds = []
    if  engine_sfx:                 lSounds.append( ("engine",     engine_sfx) );
    #if kart_sound_horn.val  != "": lSounds.append( ("horn-sound", kart_sound_horn.val ))
    #if kart_sound_crash.val != "": lSounds.append( ("crash-sound",kart_sound_crash.val))
    #if kart_sound_shoot.val != "" :lSounds.append( ("shoot-sound",kart_sound_shoot.val))
    #if kart_sound_win.val   != "" :lSounds.append( ("win-sound",  kart_sound_win.val  ))
    #if kart_sound_explode.val!="" :lSounds.append( ("explode-sound",kart_sound_explode.val))
    #if kart_sound_goo.val   != "" :lSounds.append( ("goo-sound",  kart_sound_goo.val))
    #if kart_sound_pass.val  != "" :lSounds.append( ("pass-sound", kart_sound_pass.val))
    #if kart_sound_zipper.val!= "" :lSounds.append( ("zipper-sound",kart_sound_zipper.val))
    #if kart_sound_name.val  != "" :lSounds.append( ("name-sound", kart_sound_name.val))
    #if kart_sound_attach.val!= "" :lSounds.append( ("attach-sound",kart_sound_attach.val))

    if lSounds:
        f.write('  <sounds %s = "%s"'%(lSounds[0][0], lSounds[0][1]))
        for (name, sound) in lSounds[1:]:
            f.write('\n          %s = "%s"'%(name, sound))
        f.write('/>\n')
    
# ------------------------------------------------------------------------------
# Exports the actual kart.
def exportKart(path):
  
    global the_scene
    kart_name_string = the_scene['name']
    
    if not kart_name_string or len(kart_name_string) == 0:
        log_error("No kart name specified")
        return
    
    color = the_scene['color']
    if color is None:
        log_error("Incorrect kart color")
        return

    split_color = color.split()
    if len(split_color) != 3:
        log_error("Incorrect kart color")
        return
    
    try:
        split_color[0] = "%.2f" % (int(split_color[0]) / 255.0)
        split_color[1] = "%.2f" % (int(split_color[1]) / 255.0)
        split_color[2] = "%.2f" % (int(split_color[2]) / 255.0)
    except:
        log_error("Incorrect kart color")
        return

    # Get the kart and all wheels
    # ---------------------------
    lObj          = bpy.data.objects
    lWheels       = []
    lKart         = []
    lNitroEmitter = []
    lSpeedWeighted = []
    lHeadlights = []
    hat_object = None
    for obj in lObj:
        stktype = getProperty(obj, "type", "").strip().upper()
        name    = obj.name.upper()
        if stktype=="WHEEL":
            lWheels.append(obj)
        elif stktype=="NITRO-EMITTER":
            lNitroEmitter.append(obj)
        elif stktype=="SPEED-WEIGHTED":
            lSpeedWeighted.append(obj)
        elif stktype=="IGNORE":
            pass
        elif stktype=="HEADLIGHT":
            lHeadlights.append(obj)
        elif stktype=="HAT":
            hat_object = obj
        # For backward compatibility
        #elif name in ["WHEELFRONT.R","WHEELFRONT.L", \
        #              "WHEELREAR.R", "WHEELREAR.L"     ]:
        #    lWheels.append(obj)
        else:
            # Due to limitations with the spm exporter animated
            # objects must be first in the list of objects to export:
            if obj.parent and obj.parent.type=="Armature":
                lKart.insert(0, obj)
            else:
                lKart.append(obj)
    
    # Write the xml file
    # ------------------
    kart_shadow = the_scene['shadow']
    if not kart_shadow or len(kart_shadow) == 0:
        kart_shadow = kart_name_string.lower() + "_shadow.png"
    
    kart_icon = the_scene['icon']
    if not kart_icon or len(kart_icon) == 0:
        kart_icon = kart_name_string.lower() + "_icon.png"
    
    kart_map_icon = the_scene['minimap_icon']
    if not kart_map_icon or len(kart_map_icon) == 0:
        kart_map_icon = kart_name_string.lower() + "_map_icon.png"
    
    kart_group = the_scene['group']
    if not kart_group or len(kart_group) == 0:
        kart_group = "default"
    
    kart_engine_sfx = the_scene['engine_sfx']
    if not kart_engine_sfx or len(kart_engine_sfx) == 0:
        kart_engine_sfx = "small"
    
    kart_type = 'medium'
    if 'karttype' in the_scene:
        kart_type = the_scene['karttype']
    
    f = open(path + "/kart.xml", 'w', encoding="utf-8")    
    f.write('<?xml version="1.0"?>\n')
    f.write('<!-- Generated with script from SVN rev %s -->\n'\
            % getScriptVersion())
    rgb = (0.7, 0.0, 0.0)
    model_file = kart_name_string.lower()+".spm"
    f.write('<kart name              = "%s"\n' % kart_name_string)
    f.write('      version           = "3"\n' )
    f.write('      model-file        = "%s"\n' % model_file)
    f.write('      icon-file         = "%s"\n' % kart_icon)
    f.write('      minimap-icon-file = "%s"\n' % kart_map_icon)
    f.write('      shadow-file       = "%s"\n' % kart_shadow)
    f.write('      type              = "%s"\n' % kart_type)

    center_shift = the_scene['center_shift']
    if center_shift and center_shift != 0:
        f.write('      center-shift      = "%.2f"\n' % center_shift)
        
    f.write('      groups            = "%s"\n' % kart_group)
    f.write('      rgb               = "%s %s %s" >\n' % tuple(split_color))
    
    saveSounds(f, kart_engine_sfx)
    straight_frame = saveAnimations(f)
    saveWheels(f, lWheels, path)
    saveSpeedWeighted(f, lSpeedWeighted, path, straight_frame)
    saveNitroEmitter(f, lNitroEmitter, path)
    saveHeadlights(f, lHeadlights, path, straight_frame)

    if hat_object:
        if hat_object.parent and hat_object.parent_type == 'BONE':
            if straight_frame == -1:
                print("Missing striaght frame for saving straight location")
                assert False
            bpy.context.scene.frame_set(straight_frame)
            loc, rot, scale = hat_object.matrix_world.decompose()
            rot = rot.to_euler('XZY')
            rad2deg = -180.0 / 3.1415926535;
            f.write('  <hat position="%f %f %f"\n       rotation="%f %f %f"'
                '\n       scale="%f %f %f"\n       bone="%s"/>\n' \
                % (loc[0], loc[2], loc[1], rot[0] * rad2deg, rot[2] * rad2deg, rot[1] * rad2deg,\
                scale[0], scale[2], scale[1], hat_object.parent_bone))
        else:
            loc, rot, scale = hat_object.matrix_world.decompose()
            rad2deg = -180.0 / 3.1415926535;
            rot = rot.to_euler('XZY')
            f.write('  <hat position="%f %f %f"\n       rotation="%f %f %f"'
                '\n       scale="%f %f %f"/>\n' \
                % (loc[0], loc[2], loc[1], rot[0] * rad2deg, rot[2] * rad2deg, rot[1] * rad2deg,\
                scale[0], scale[2], scale[1]))

    if 'kartLean' in the_scene and len(the_scene['kartLean']) > 0:
        f.write('  <lean max="' + the_scene['kartLean'] + '"/>\n')
    if 'exhaust_xml' in the_scene and len(the_scene['exhaust_xml']) > 0:
        f.write('  <exhaust file="' + the_scene['exhaust_xml'] + '"/>\n')

    f.write('</kart>\n')
    f.close()

    the_scene.obj_list = lKart
    
    if 'spm_export' not in dir(bpy.ops.screen):
        log_error("Cannot find the spm exporter, make sure you installed it properly")
        return
    
    bpy.ops.screen.spm_export(localsp=False, filepath=path+"/"+model_file,
                              export_tangent='precalculate_tangents' in bpy.context.scene\
                              and bpy.context.scene['precalculate_tangents'] == 'true',
                              overwrite_without_asking=True, static_mesh_frame = straight_frame)
    the_scene.obj_list = []
    
    #spm_export.write_spm_file(Blender.sys.join(path, model_file), lKart)
    
    # materials file
    # ----------
    if 'stk_material_exporter' not in dir(bpy.ops.screen):
        log_error("Cannot find the material exporter, make sure you installed it properly")
        return
    
    bpy.ops.screen.stk_material_exporter(filepath=path)
    
    import datetime
    now = datetime.datetime.now()
    log_info("Export completed on " + now.strftime("%Y-%m-%d %H:%M"))



# ==============================================================================
def savescene_callback(path):
    global log
    log = []
    
    exporter = exportKart(path)


# ==== EXPORT OPERATOR ====
class STK_Kart_Export_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_kart_export")
    bl_label = ("SuperTuxKart Kart Export")
    filepath = bpy.props.StringProperty(subtype="FILE_PATH")

    def invoke(self, context, event):
        
        if bpy.context.mode != 'OBJECT':
            self.report({'ERROR'}, "You must be in object mode")
            log_error("You must be in object mode")
            return {'FINISHED'}
        
        if 'is_stk_kart' not in context.scene or context.scene['is_stk_kart'] != 'true':
            log_error("Not a STK kart!")
            return {'FINISHED'}
            
        blend_filepath = context.blend_data.filepath
        if not blend_filepath:
            blend_filepath = "Untitled"
        else:
            import os
            blend_filepath = os.path.splitext(blend_filepath)[0]
        self.filepath = blend_filepath
        
        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}

    def execute(self, context):
        
        if bpy.context.mode != 'OBJECT':
            self.report({'ERROR'}, "You must be in object mode")
            log_error("You must be in object mode")
            return {'FINISHED'}
        
        if self.filepath == "" or 'is_stk_kart' not in context.scene or context.scene['is_stk_kart'] != 'true':
            return {'FINISHED'}
        
        global operator
        operator = self
        
        # FIXME: silly and ugly hack, the list of objects to export is passed through
        #        a custom scene property
        # FIXME: both the kart export script and the track export script do this!! conflicts in sight?
        bpy.types.Scene.obj_list = property(getlist, setlist)
        
        import os.path
        savescene_callback(os.path.dirname(self.filepath))
        return {'FINISHED'}

class STK_Copy_Log_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_kart_copy_log")
    bl_label = ("Copy Log")

    def execute(self, context):
        global log
        bpy.data.window_managers[0].clipboard = str(log)
        return {'FINISHED'}
      
class STK_Clean_Log_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_kart_clean_log")
    bl_label = ("Clean Log")

    def execute(self, context):
        global log
        log = []
        print("Log cleaned")
        return {'FINISHED'}

# ==== PANEL ====
class STK_Kart_Exporter_Panel(bpy.types.Panel):
    bl_label = "Kart Exporter"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    
    def draw(self, context):
        global the_scene
        the_scene = context.scene
        
        layout = self.layout
        
        # ==== Types group ====
        row = layout.row()
        
        row.operator("screen.stk_kart_export", "Export", icon='AUTO')
        
        if bpy.context.mode != 'OBJECT':
            row.enabled = False
        
        # ==== Output Log ====
        
        global log
        
        if len(log) > 0:
            box = layout.box()
            row = box.row()
            row.label("Log")
            
            for type,msg in log:
                if type == 'INFO':
                  row = box.row()
                  row.label(msg, icon='INFO')
                elif type == 'WARNING':
                  row = box.row()
                  row.label("WARNING: " + msg, icon='ERROR')
                elif type == 'ERROR':
                  row = box.row()
                  row.label("ERROR: " + msg, icon='CANCEL')
            
            row = box.row()
            row.operator("screen.stk_kart_clean_log", text="Clear Log", icon='X')
            row.operator("screen.stk_kart_copy_log",  text="Copy Log", icon='COPYDOWN')
              
# Add to a menu
def menu_func_export(self, context):
    global the_scene
    the_scene = context.scene
    self.layout.operator(STK_Kart_Export_Operator.bl_idname, text="STK Kart")

def register():
    bpy.types.INFO_MT_file_export.append(menu_func_export)
    bpy.utils.register_module(__name__)

def unregister():
    bpy.types.INFO_MT_file_export.remove(menu_func_export)

if __name__ == "__main__":
    register()
