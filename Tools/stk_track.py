#!BPY

"""
Name: 'STK Track Exporter (.track)...'
Blender: 259
Group: 'Export'
Tooltip: 'Export a SuperTuxKart track scene'
"""
__author__ = ["Joerg Henrichs (hiker), Marianne Gagnon (Auria)"]
__url__ = ["supertuxkart.sourceforge.net"]
__version__ = "$Revision: 17016 $"
__bpydoc__ = """\
"""

# From Supertuxkart SVN revision $Revision: 17016 $

# Copyright (C) 2010 Joerg Henrichs
# Copyright (C) 2012 Marianne Gagnon
# INSERT (C) here!

#If you get an error here, it might be
#because you don't have Python installed.
import bpy
import sys, os, os.path, struct, math, string, re
import random


track_tesselated_objects = {}

def track_tesselate_if_needed(objdata):
    if objdata not in track_tesselated_objects:
        objdata.calc_tessface()
        track_tesselated_objects[objdata] = True
    return objdata

def track_getFaces(obj_data):
    # BMesh in blender 2.63 broke this
    if bpy.app.version[1] >= 63:
        return track_tesselate_if_needed(obj_data).tessfaces
    else:
        return obj_data.faces

def track_getUVTextures(obj_data):
    # BMesh in blender 2.63 broke this
    if bpy.app.version[1] >= 63:
        return track_tesselate_if_needed(obj_data).tessface_uv_textures
    else:
        return obj_data.uv_textures
        
bl_info = {
    "name": "SuperTuxKart Track Exporter",
    "description": "Exports a blender scene to the SuperTuxKart track format",
    "author": "Joerg Henrichs, Marianne Gagnon",
    "version": (1,0),
    "blender": (2, 5, 9),
    "api": 31236,
    "location": "File > Export",
    "warning": '', # used for warning icon and text in addons panel
    "wiki_url": "http://supertuxkart.sourceforge.net/Get_involved",
    "tracker_url": "https://sourceforge.net/apps/trac/supertuxkart/",
    "category": "Import-Export"}

from mathutils import *

operator = None
the_scene = None


log = []

def log_info(msg):
    print("INFO:", msg)
    log.append( ('INFO', msg) )
def log_warning(msg):
    print("WARNING:", msg)
    log.append( ('WARNING', msg) )
def log_error(msg):
    print("ERROR:", msg)
    log.append( ('ERROR', msg) )
    
if not hasattr(sys, "argv"):
    sys.argv = m["???"]

def getScriptVersion():
    try:
        m = re.search('(\d+)', __version__)
        return str(m.group(0))
    except:
        return "Unknown"

def writeIPO(f, anim_data ):
    #dInterp = {IpoCurve.InterpTypes.BEZIER:        "bezier",
    #           IpoCurve.InterpTypes.LINEAR:        "linear",
    #           IpoCurve.InterpTypes.CONST:         "const"          }
    #dExtend = {IpoCurve.ExtendTypes.CONST:         "const",
    #           IpoCurve.ExtendTypes.EXTRAP:        "extrap",
    #           IpoCurve.ExtendTypes.CYCLIC_EXTRAP: "cyclic_extrap",
    #           IpoCurve.ExtendTypes.CYCLIC:        "cyclic"         }
    
    if anim_data and anim_data.action:
        ipo = anim_data.action.fcurves
    else:
        return
    
    # ==== Possible values returned by blender ====
    # fcurves[0].data_path
    #    location, rotation_euler, scale
    # fcurves[0].extrapolation
    #    CONSTANT, LINEART
    # fcurves[0].keyframe_points[0].interpolation
    #    CONSTANT, LINEAR, BEZIER
    
    # Swap Y and Z axis
    axes = ['X', 'Z', 'Y']
    
    for curve in ipo:
        
        if curve.data_path == 'location':
            name = "Loc" + axes[curve.array_index]
        elif curve.data_path == 'rotation_euler':
            name = "Rot" + axes[curve.array_index]
        elif curve.data_path == 'scale':
            name = "Scale" + axes[curve.array_index]
        else:
            if "pose.bones" not in curve.data_path: # we ignore bone curves
                log_warning("Unknown curve type " + curve.data_path)
            continue
        
        extrapolation = "const"
        
        for modifier in curve.modifiers:
            if modifier.type == 'CYCLES':
                extrapolation = "cyclic"
                break
        
        # If any point is bezier we'll export as Bezier
        interpolation = "linear"
        for bez in curve.keyframe_points:
            if bez.interpolation=='BEZIER':
                interpolation = "bezier"
                break
        
        # Rotations are stored in randians
        if name[:3]=="Rot":
            factor=-57.29577951 # 180/PI
        else:
            factor=1
        f.write("    <curve channel=\"%s\" interpolation=\"%s\" extend=\"%s\">\n"% \
                (name, interpolation, extrapolation))
                #(name, dInterp[curve.interpolation], dExtend[curve.extend]))
        
        warning_shown = False
        
        for bez in curve.keyframe_points:
            if interpolation=="bezier":
                if bez.interpolation=='BEZIER':
                    f.write("      <p c=\"%.3f %.3f\" h1=\"%.3f %.3f\" h2=\"%.3f %.3f\"/>\n"%\
                            (bez.co[0],factor*bez.co[1],
                                bez.handle_left[0], factor*bez.handle_left[1],
                                bez.handle_right[0], factor*bez.handle_right[1]))
                else:
                    # point with linear IPO in bezier curve
                    f.write("      <p c=\"%.3f %.3f\" h1=\"%.3f %.3f\" h2=\"%.3f %.3f\"/>\n"%\
                            (bez.co[0], factor*bez.co[1],
                                bez.co[0] - 1, factor*bez.co[1],
                                bez.co[0] + 1, factor*bez.co[1]))
                    
                    if not warning_shown:
                        log_warning("You have an animation curve which contains a mix of mixture of Bezier and " +
                                    "linear interpolation, please convert everything to Bezier for best results")
                        warning_shown = True
            else:
                f.write("      <p c=\"%.3f %.3f\"/>\n"%(bez.co[0],
                                                        factor*bez.co[1]))
        f.write("    </curve>\n")
            

# --------------------------------------------------------------------------

def writeBezierCurve(f, curve, speed, extend="cyclic"):
    matrix = curve.matrix_world
    if len(curve.data.splines) > 1:
        log_warning(curve.name + " contains multiple curves, will only export the first one")
    
    f.write('    <curve channel="LocXYZ" speed="%.2f" interpolation="bezier" extend="%s">\n'\
            %(speed, extend))
    if curve.data.splines[0].type != 'BEZIER':
        log_warning(curve.name + " should be a bezier curve, not a " + curve.data.splines[0].type)
    else:
        for pt in curve.data.splines[0].bezier_points:
            v0 = matrix*pt.handle_left
            v1 = matrix*pt.co*matrix 
            v2 = matrix*pt.handle_right
            f.write("      <point c=\"%f %f %f\" h1=\"%f %f %f\" h2=\"%f %f %f\" />\n"% \
                    ( v1[0],v1[2],v1[1],
                      v0[0],v0[2],v0[1],
                      v2[0],v2[2],v2[1] ) )
    f.write("    </curve>\n")
        
    
# ------------------------------------------------------------------------------
# Checks if there are any animated textures in any of the objects in the
# list l.
def checkForAnimatedTextures(lObjects):
    lAnimTextures = []
    for obj in lObjects:
        use_anim_texture = getObjectProperty(obj, "enable_anim_texture", "false")
        if use_anim_texture != 'true': continue
        
        anim_texture = getObjectProperty(obj, "anim_texture", None)
        
        if anim_texture is None or len(anim_texture) == 0:
            log_warning("object %s has an invalid animated-texture configuration" % obj.name)
            continue
        #if anim_texture == 'stk_animated_mudpot_a.png':
        print('Animated texture {} in {}.'.format(anim_texture, obj.name))
        dx = getObjectProperty(obj, "anim_dx", 0)
        dy = getObjectProperty(obj, "anim_dy", 0)
        dt = getObjectProperty(obj, "anim_dt", 0)
        
        use_anim_texture_by_step = getObjectProperty(obj, "enable_anim_by_step", "false")
        
        lAnimTextures.append( (anim_texture, dx, dy, dt, use_anim_texture_by_step) )
    return lAnimTextures

# ------------------------------------------------------------------------------
def writeAnimatedTextures(f, lAnimTextures):
    for (name, dx, dy, dt, use_anime_texture_by_step) in lAnimTextures:
        
        sdt=""
        if use_anime_texture_by_step == "true":
            sdt = ' animByStep="true" dt="%.3f" '%float(dt)
            dy = 1.0/dy
        
        sdx=""
        if dx: sdx = " dx=\"%.5f\" "%float(dx)
        sdy=""
        if dy: sdy = " dy=\"%.5f\" "%float(dy)
        
        if name is None or len(name) == 0:
            continue
        f.write("    <animated-texture name=\"%s\"%s%s%s/>\n"%(name, sdx, sdy, sdt) )

# ------------------------------------------------------------------------------
def Round(f):
    r = round(f,6) # precision set to 10e-06
    if r == int(r):
        return str(int(r))
    else:
        return str(r)

# ------------------------------------------------------------------------------
# Gets a custom property of a scene, returning the default if the id property
# is not set. If set_value_if_undefined is set and the property is not
# defined, this function will also set the property to this default value.
def getSceneProperty(scene, name, default="", set_value_if_undefined=1):
    import traceback
    try:
        prop = scene[name]
        if isinstance(prop, str):
            from xml.sax.saxutils import escape
            # + "" is used to force a copy of the string AND to convert from binary format to string format
            # escape formats the string for XML
            return (escape(prop + "") + "").encode('ascii', 'xmlcharrefreplace').decode("ascii")
        else:
            return prop
    except:
        if default!=None and set_value_if_undefined:
            scene[name] = default
    return default

# ------------------------------------------------------------------------------
# Gets a custom property of an object
def getObjectProperty(obj, name, default=""):
    if obj.proxy is not None:
        try:
            return obj.proxy[name]
        except:
            pass
        
    try:
        return obj[name]
    except:
        return default

# ------------------------------------------------------------------------------
# FIXME: should use xyz="..." format
# Returns a string 'x="1" y="2" z="3" h="4"', where 1, 2, ...are the actual
# location and rotation of the given object. The location has a swapped
# y and z axis (so that the same coordinate system as in-game is used).
def getXYZHString(obj):
    loc     = obj.location
    hpr     = obj.rotation_euler
    rad2deg = 180.0/3.1415926535;
    s="x=\"%.2f\" y=\"%.2f\" z=\"%.2f\" h=\"%.2f\"" %\
       (loc[0], loc[2], loc[1], -hpr[2]*rad2deg)
    return s

# ------------------------------------------------------------------------------
# Returns a string 'xyz="1 2 3" h="4"', where 1, 2, ...are the actual
# location and rotation of the given object. The location has a swapped
# y and z axis (so that the same coordinate system as in-game is used).
def getNewXYZHString(obj):
    loc     = obj.location
    hpr     = obj.rotation_euler
    rad2deg = 180.0/3.1415926535;
    s="xyz=\"%.2f %.2f %.2f\" h=\"%.2f\"" %\
       (loc[0], loc[2], loc[1], hpr[2]*rad2deg)
    return s
    
# ------------------------------------------------------------------------------
# Returns a string 'xyz="1 2 3" hpr="4 5 6"' where 1,2,... are the actual
# location and rotation of the given object. The location has a swapped
# y and z axis (so that the same coordinate system as in-game is used), and
# rotations are multiplied by 10 (since bullet stores the values in units
# of 10 degrees.)
def getXYZHPRString(obj):
    loc     = obj.location
    # irrlicht uses XZY
    hpr     = obj.rotation_euler.to_quaternion().to_euler('XZY')
    si      = obj.scale
    rad2deg = 180.0/3.1415926535;
    s="xyz=\"%.2f %.2f %.2f\" hpr=\"%.1f %.1f %.1f\" scale=\"%.2f %.2f %.2f\"" %\
       (loc[0], loc[2], loc[1], -hpr[0]*rad2deg, -hpr[2]*rad2deg,
        -hpr[1]*rad2deg, si[0], si[2], si[1])
    return s

    
# ------------------------------------------------------------------------------
def getXYZString(obj):
    loc = obj.location
    s = "xyz=\"%.2f %.2f %.2f\"" % (loc[0], loc[2], loc[1])
    return s
    
# --------------------------------------------------------------------------
# Write several ways of writing true/false as Y/N
def convertTextToYN(sText):
    sTemp = sText.strip().upper()
    if sTemp=="0" or sTemp[0]=="N" or sTemp=="FALSE":
        return "N"
    else:
        return "Y"

# ------------------------------------------------------------------------------
# OBSOLETE
class WaterExporter:
    
    def __init__(self, parentTrackExporter, sPath):
        self.m_parent_track_exporter = parentTrackExporter;
        self.m_export_path = sPath
        self.m_objects = []
    
    def processObject(self, object, stktype):
        if stktype=="WATER":
            log_warning("Water object type is obsolete and should not be used : <%s>" % object.name)
            self.m_objects.append(object)
            return True
        else:
            return False
    
    def export(self, f):
        for obj in self.m_objects:
            name     = getObjectProperty(obj, "name",   obj.name )
            if len(name) == 0:
                name = obj.name
            height   = getObjectProperty(obj, "height", None     )
            speed    = getObjectProperty(obj, "speed",  None     )
            length   = getObjectProperty(obj, "length", None     )
            lAnim    = checkForAnimatedTextures([obj])
            spm_name = self.m_parent_track_exporter.exportLocalSPM(obj, self.m_export_path, name, True)
            s = "  <water model=\"%s\" %s" % (spm_name, getXYZHPRString(obj))
            if height: s = "%s height=\"%.2f\""%(s, float(height))
            if speed:  s = "%s speed=\"%.2f\"" %(s, float(speed))
            if length: s = "%s length=\"%.2f\""%(s, float(length))
            if lAnim:
                f.write("%s>\n" % s)
                writeAnimatedTextures(f, lAnim)
                f.write("  </water>\n")
            else:
                f.write("%s/>\n" % s);

           
# ------------------------------------------------------------------------------
class ItemsExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.type=="EMPTY":
            # For backward compatibility test for the blender name
            # in case that there is no type property defined. This makes
            # it easier to port old style tracks without having to
            # add the property for all items.
            stktype = getObjectProperty(object, "type", object.name).upper()
            # Check for old and new style names
            if stktype[:8] in ["GHERRING", "RHERRING", "YHERRING", "SHERRING"] \
                or stktype[: 6]== "BANANA"     or stktype[:4]=="ITEM"           \
                or stktype[:11]=="NITRO-SMALL" or stktype[:9]=="NITRO-BIG"      \
                or stktype[:11]=="NITRO_SMALL" or stktype[:9]=="NITRO_BIG"      \
                or stktype[:11]=="SMALL-NITRO" or stktype[:9]=="BIG-NITRO"      \
                or stktype[: 6]=="ZIPPER":
                self.m_objects.append(object)
                return True
        return False

    def export(self, f):
        rad2deg = 180.0/3.1415926535
        global the_scene
        scene = the_scene
        is_ctf = getSceneProperty(scene, "ctf",  "false") == "true"
        for obj in self.m_objects:
            item_type = getObjectProperty(obj, "type", "").lower()
            if item_type=="":
                # If the type is not specified in the property,
                # assume it's an old style item, which means the
                # blender object name is to be used
                l = obj.name.split(".")
                if len(l)!=1:
                    if l[-1].isdigit():   # Remove number appended by blender
                        l = l[:-1]
                    item_type = ".".join(l)
                else:
                    item_type = obj.name
                # Portability for old models:
                g=re.match("(.*) *{(.*)}", item_type)
                if g:
                    item_type = g.group(1)
                    specs = g.group(2).lower()
                    if specs.find("z")>=0: z=None
                    if specs.find("p")>=0: p=None
                    if specs.find("r")>=0: r=None
                if item_type=="GHERRING": item_type="banana"
                if item_type=="RHERRING": item_type="item"
                if item_type=="YHERRING": item_type="big-nitro"
                if item_type=="SHERRING": item_type="small-nitro"
            else:
                if item_type=="nitro-big": item_type="big-nitro"
                if item_type=="nitro_big": item_type="big-nitro"
                if item_type=="nitro-small": item_type="small-nitro"
                if item_type=="nitro_small": item_type="small-nitro"

            # Get the position of the item - first check if the item should
            # be dropped on the track, or stay at the position indicated.
            rx,ry,rz = map(lambda x: rad2deg*x, obj.rotation_euler)
            h,p,r    = map(lambda i: "%.2f"%i, [rz,rx,ry])
            x,y,z    = map(lambda i: "%.2f"%i, obj.location)
            drop     = getObjectProperty(obj, "dropitem", "true").lower()
            # Swap y and z axis to have the same coordinate system used in game.
            s        = "%s id=\"%s\" x=\"%s\" y=\"%s\" z=\"%s\"" % (item_type, obj.name, x, z, y)
            if h and h!="0.00": s = "%s h=\"%s\""%(s, h)
            if drop=="false":
                # Pitch and roll will be set automatically if dropped
                if p and p!="0.00": s="%s p=\"%s\""%(s, p)
                if r and r!="0.00": s="%s r=\"%s\""%(s, r)
                s="%s drop=\"false\""%s
            if is_ctf:
                f.write("  <%s ctf=\"%s\"/>\n" % (s, getObjectProperty(obj, "ctf_only", "false").lower()))
            else:
                f.write("  <%s />\n" % s)
# ------------------------------------------------------------------------------
class ParticleEmitterExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.type=="EMPTY" and stktype=="PARTICLE_EMITTER":
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        for obj in self.m_objects:
            try:
                originXYZ = getNewXYZHString(obj)
                
                flags = []
                if len(getObjectProperty(obj, "particle_condition", "")) > 0:
                    flags.append('conditions="' + getObjectProperty(obj, "particle_condition", "") + '"')
                
                if getObjectProperty(obj, "clip_distance", 0) > 0 :
                    flags.append('clip_distance="%i"' % getObjectProperty(obj, "clip_distance", 0))
                    
                if getObjectProperty(obj, "auto_emit", 'true') == 'false':
                    flags.append('auto_emit="%s"' % getObjectProperty(obj, "auto_emit", 'true'))
                
                f.write('  <particle-emitter kind="%s" id=\"%s\" %s %s>\n' %\
                        (getObjectProperty(obj, "kind", 0), obj.name, originXYZ, ' '.join(flags)))
                
                if obj.animation_data and obj.animation_data.action and obj.animation_data.action.fcurves and len(obj.animation_data.action.fcurves) > 0:
                    writeIPO(f, obj.animation_data)
                
                f.write('  </particle-emitter>\n')
            except:
                log_error("Invalid particle emitter <" + getObjectProperty(obj, "name", obj.name) + "> ")

# ------------------------------------------------------------------------------
# Blender hair systems are usually used to automate the placement of plants on the ground
class BlenderHairExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.particle_systems is not None and len(object.particle_systems) >= 1 and \
           object.particle_systems[0].settings.type == 'EMITTER':
            if (object.particle_systems[0].settings.dupli_object is not None) or \
               (object.particle_systems[0].settings.dupli_group is not None): #and getObjectProperty(object.particle_systems[0].settings.dupli_object, "type", "") == "object":
                self.m_objects.append(object)
            else:
                log_warning("Ignoring invalid hair system <%s>" % object.name)

        return False # always return false so that the object is exported normally as a mesh too
        
    def export(self, f):
        rad2deg = 180.0/3.1415926535;
        
        for obj in self.m_objects:
        
            for particleSystem in obj.particle_systems:
        
                f.write('  <!-- Hair system %s, contains %i particles -->\n' % (obj.name, len(particleSystem.particles)))

                for particle in particleSystem.particles:
                    if particleSystem.settings.render_type == 'OBJECT':
                        duplicated_obj = particleSystem.settings.dupli_object
                    # Currently we only support random picking from the group
                    elif particleSystem.settings.render_type == 'GROUP':
                        object_group = particleSystem.settings.dupli_group.objects
                        choice = random.randint(0, len(object_group) - 1)
                        duplicated_obj = object_group[choice]

                    loc = particle.location
                    hpr = particle.rotation.to_euler('XYZ')
                    
                    # hack to get proper orientation
                    if (particleSystem.settings.normal_factor >= 0.5):
                        hpr.rotate_axis("Z", -1.57079633)

                    #print (particle.size)
                    si = particle.size #/ duplicated_obj.dimensions[2]
                    loc_rot_scale_str = "xyz=\"%.2f %.2f %.2f\" hpr=\"%.1f %.1f %.1f\" scale=\"%.2f %.2f %.2f\"" %\
                       (loc[0], loc[2], loc[1], -hpr[0]*rad2deg, -hpr[2]*rad2deg,
                        -hpr[1]*rad2deg, si, si, si)
                    
                    if duplicated_obj.proxy is not None and duplicated_obj.proxy.library is not None:
                        path_parts = re.split("/|\\\\", duplicated_obj.proxy.library.filepath)
                        lib_name = path_parts[-2]
                        f.write('  <library name="%s" id=\"%s\" %s/>\n' % (lib_name, duplicated_obj.name, loc_rot_scale_str))
                    else:
                        name     = getObjectProperty(duplicated_obj, "name",   duplicated_obj.name )
                        if len(name) == 0:
                            name = duplicated_obj.name
                        f.write('  <object type="animation" %s interaction="ghost" model="%s.spm" skeletal-animation="false"></object>\n' % (loc_rot_scale_str, name))
                    
            f.write('  <!-- END Hair system %s -->\n\n' % obj.name)
        
    
# ------------------------------------------------------------------------------
class SoundEmitterExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.type=="EMPTY" and stktype=="SFX_EMITTER":
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        for obj in self.m_objects:
            try:
                # origin
                originXYZ = getXYZHPRString(obj)
                
                play_near_string = ""
                if getObjectProperty(obj, "play_when_near", "false") == "true":
                    dist = getObjectProperty(obj, "play_distance", 1.0)
                    play_near_string = " play-when-near=\"true\" distance=\"%.1f\"" % dist
                
                conditions_string = ""
                if len(getObjectProperty(obj, "sfx_conditions", "")) > 0:
                    conditions_string = ' conditions="' + getObjectProperty(obj, "sfx_conditions", "") + '"'
                
                
                f.write('  <object type="sfx-emitter" id=\"%s\" sound="%s" rolloff="%.3f" volume="%s" max_dist="%.1f" %s%s%s>\n' %\
                        (obj.name,
                         getObjectProperty(obj, "sfx_filename", "some_sound.ogg"),
                         getObjectProperty(obj, "sfx_rolloff", 0.05),
                         getObjectProperty(obj, "sfx_volume", 0),
                         getObjectProperty(obj, "sfx_max_dist", 500.0), originXYZ, play_near_string, conditions_string))
                
                if obj.animation_data and obj.animation_data.action and obj.animation_data.action.fcurves and len(obj.animation_data.action.fcurves) > 0:
                    writeIPO(f, obj.animation_data)
                
                f.write('  </object>\n')
            except:
                log_error("Invalid sound emitter <" + getObjectProperty(obj, "name", obj.name) + "> ")
                
        
# ------------------------------------------------------------------------------
class ActionTriggerExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if stktype=="ACTION_TRIGGER":
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        for obj in self.m_objects:
            try:
                # origin
                originXYZ = getXYZHPRString(obj)
                trigger_type = getObjectProperty(obj, "trigger_type", "point")
                
                #if trigger_type == "sphere":
                #    radius = (obj.dimensions.x + obj.dimensions.y + obj.dimensions.z)/6 # divide by 3 to get average size, divide by 2 to get radius from diameter
                #    f.write("    <check-sphere xyz=\"%.2f %.2f %.2f\" radius=\"%.2f\"/>\n" % \
                #            (obj.location[0], obj.location[2], obj.location[1], radius) )
                if trigger_type == "point":
                    f.write('  <object type="action-trigger" trigger-type="point" id=\"%s\" action="%s" distance="%s" reenable-timeout="%s" triggered-object="%s" %s/>\n' %\
                        (obj.name,
                         getObjectProperty(obj, "action", ""),
                         getObjectProperty(obj, "trigger_distance", 5.0),
                         getObjectProperty(obj, "reenable_timeout", 999999.9),
                         getObjectProperty(obj, "triggered_object", ""),
                         originXYZ))
                elif trigger_type == "cylinder":
                    radius = (obj.dimensions.x + obj.dimensions.y)/4 # divide by 2 to get average size, divide by 2 to get radius from diameter
                    f.write("  <object type=\"action-trigger\" trigger-type=\"cylinder\" action=\"%s\" xyz=\"%.2f %.2f %.2f\" radius=\"%.2f\" height=\"%.2f\"/>\n" % \
                            (getObjectProperty(obj, "action", ""), obj.location[0], obj.location[2], obj.location[1], radius, obj.dimensions.z) )
            except:
                log_error("Invalid action <" + getObjectProperty(obj, "name", obj.name) + "> ")

        
# ------------------------------------------------------------------------------
class StartPositionFlagExporter:
    
    def __init__(self):
        self.m_objects = []
        self.m_red_flag = None
        self.m_blue_flag = None

    def processObject(self, object, stktype):
        if object.type=="EMPTY" and stktype[:5]=="START":
            self.m_objects.append(object)
            return True
        elif object.type=="EMPTY" and stktype[:8]=="RED_FLAG":
            self.m_red_flag = object
            return True
        elif object.type=="EMPTY" and stktype[:9]=="BLUE_FLAG":
            self.m_blue_flag = object
            return True
        else:
            return False
            
    def export(self, f):
        global the_scene
        scene = the_scene
        karts_per_row      = int(getSceneProperty(scene, "start_karts_per_row",      2))
        distance_forwards  = float(getSceneProperty(scene, "start_forwards_distance",  1.5))
        distance_sidewards = float(getSceneProperty(scene, "start_sidewards_distance", 3.0))
        distance_upwards   = float(getSceneProperty(scene, "start_upwards_distance",   0.1))
        if getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true':
            f.write("  <default-start karts-per-row     =\"%i\"\n"%karts_per_row     )
            f.write("                 forwards-distance =\"%.2f\"\n"%distance_forwards )
            f.write("                 sidewards-distance=\"%.2f\"\n"%distance_sidewards)
            f.write("                 upwards-distance  =\"%.2f\"/>\n"%distance_upwards)

        is_ctf = self.m_red_flag is not None and self.m_blue_flag is not None \
            and getSceneProperty(scene, "ctf",  "false") == "true"
        dId2Obj_ctf = {}
        dId2Obj = {}
        for obj in self.m_objects:
            stktype = getObjectProperty(obj, "type", obj.name).upper()
            id = int(getObjectProperty(obj, "start_index", "-1"))
            if id == "-1":
                log_warning("Invalid start position " + id)
            if is_ctf and getObjectProperty(obj, "ctf_only", "false").lower() == "true":
                dId2Obj_ctf[id] = obj
            else:
                dId2Obj[id] = obj

        l = dId2Obj.keys()

        if len(l) < 4 and getSceneProperty(scene, "arena",  "false") == "true":
            log_warning("You should define at least 4 start positions")
        if is_ctf and len(dId2Obj_ctf.keys()) < 16:
            log_warning("You should define at least 16 ctf start positions, odd"
                " / even index alternatively for blue and red team.")

        for key, value in sorted(dId2Obj.items()):
            f.write("  <start %s/>\n"%getXYZHString(value))
        for key, value in sorted(dId2Obj_ctf.items()):
            f.write("  <ctf-start %s/>\n"%getXYZHString(value))
        if is_ctf:
            f.write("  <red-flag %s/>\n"%getXYZHString(self.m_red_flag))
            f.write("  <blue-flag %s/>\n"%getXYZHString(self.m_blue_flag))

# ------------------------------------------------------------------------------
class LibraryNodeExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.proxy is not None and object.proxy.library is not None:
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        import re
        for obj in self.m_objects:
            try:
                path_parts = re.split("/|\\\\", obj.proxy.library.filepath)
                lib_name = path_parts[-2]

                # origin
                originXYZ = getXYZHPRString(obj)
                
                f.write('  <library name="%s" id=\"%s\" %s>\n' % (lib_name, obj.name, originXYZ))
                if obj.animation_data and obj.animation_data.action and obj.animation_data.action.fcurves and len(obj.animation_data.action.fcurves) > 0:
                    writeIPO(f, obj.animation_data)
                f.write('  </library>\n')
            except:
                log_error("Invalid linked object <" + getObjectProperty(obj, "name", obj.name) + "> ")

                
# ------------------------------------------------------------------------------
class BillboardExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if stktype=="BILLBOARD":
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        for obj in self.m_objects:
            data = obj.data
            
            # check the face
            face_len = len(track_getFaces(data))
            if face_len == 0:
                log_error("Billboard <" + getObjectProperty(obj, "name", obj.name) \
                    + "> must have at least one face")
                return
            if face_len > 1:
                log_error("Billboard <" + getObjectProperty(obj, "name", obj.name) \
                    + "> has more than ONE face")
                return
            
            # check the points
            if len(track_getFaces(data)[0].vertices) > 4:
                log_error("Billboard <" + getObjectProperty(obj, "name", obj.name)\
                        + "> has more than 4 points")
                return
            
            if len(track_getUVTextures(data)) < 1 or len(track_getUVTextures(data)[0].data) < 1:
                log_error("Billboard <" + getObjectProperty(obj, "name", obj.name)\
                        + "> has no UV texture")
                return
            
            
            try:
                # write in the XML
                # calcul the size and the position
                x_min = data.vertices[0].co[0]
                x_max = x_min
                y_min = data.vertices[0].co[2]
                y_max = y_min
                z_min = data.vertices[0].co[1]
                z_max = z_min
                for i in range(1, 4):
                    x_min = min(x_min, data.vertices[i].co[0])
                    x_max = max(x_max, data.vertices[i].co[0])
                    y_min = min(y_min, data.vertices[i].co[2])
                    y_max = max(y_max, data.vertices[i].co[2])
                    z_min = min(z_min, data.vertices[i].co[1])
                    z_max = max(z_max, data.vertices[i].co[1])
                
                fadeout_str = ""
                fadeout = getObjectProperty(obj, "fadeout", "false")
                if fadeout == "true":
                    start = float(getObjectProperty(obj, "start", 1.0))
                    end = float(getObjectProperty(obj, "end", 15.0))
                    fadeout_str = "fadeout=\"true\" start=\"%.2f\" end=\"%.2f\""%(start,end)
                
                uv = track_getUVTextures(data)
                f.write('  <object type="billboard" id=\"%s\" texture="%s" xyz="%.2f %.2f %.2f" \n'%
                        (obj.name, os.path.basename(uv[0].data[0].image.filepath),
                        obj.location[0], obj.location[2], obj.location[1]) )
                f.write('             width="%.3f" height="%.3f" %s>\n' %(max(x_max-x_min, z_max-z_min), y_max-y_min, fadeout_str) )
                if obj.animation_data and obj.animation_data.action and obj.animation_data.action.fcurves and len(obj.animation_data.action.fcurves) > 0:
                    writeIPO(f, obj.animation_data)
                f.write('  </object>\n')

            except:
                log_error("Invalid billboard <" + getObjectProperty(obj, "name", obj.name) + "> ")
                

# ------------------------------------------------------------------------------
class LightsExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.type=="LAMP" and stktype == "LIGHT":
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        for obj in self.m_objects:
            colR = int(obj.data.color[0] * 255)
            colG = int(obj.data.color[1] * 255)
            colB = int(obj.data.color[2] * 255)

            f.write('  <light %s id=\"%s\" distance="%.2f" energy="%.2f" color="%i %i %i"' \
                    % (getXYZString(obj), obj.name, obj.data.distance, obj.data.energy, colR, colG, colB))
            if_condition = getObjectProperty(obj, "if", "")
            if len(if_condition) > 0:
                f.write(' if=\"%s\"' % if_condition)
            f.write('>\n')
            if obj.animation_data and obj.animation_data.action and obj.animation_data.action.fcurves and len(obj.animation_data.action.fcurves) > 0:
                writeIPO(f, obj.animation_data)
            f.write('  </light>\n')

# ------------------------------------------------------------------------------
class LightShaftExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if object.type=="LAMP" and stktype == "LIGHTSHAFT_EMITTER":
            self.m_objects.append(object)
            return True
        else:
            return False
            
    def export(self, f):
        for obj in self.m_objects:
            f.write('  <lightshaft %s id=\"%s\" opacity="%.2f" color="%s"/>\n' \
                    % (getXYZString(obj), obj.name, getObjectProperty(obj, "lightshaft_opacity", 0.7), getObjectProperty(obj, "lightshaft_color", "255 255 255")))
           
# ------------------------------------------------------------------------------
class NavmeshExporter:
    
    def __init__(self):
        self.m_objects = []
    
    def processObject(self, object, stktype):
        
        if stktype=="NAVMESH":
            is_arena = getSceneProperty(bpy.data.scenes[0], "arena", "false") == "true"
            is_soccer = getSceneProperty(bpy.data.scenes[0], "soccer", "false") == "true"
            if (is_arena or is_soccer):
                self.m_objects.append(object)
            else:
                log_warning("Navmesh may only be used in battle arenas or soccer field")
                
            if len(self.m_objects) > 1:
                log_warning("Cannot have more than 1 navmesh")
                
            print("exportNavmesh 1")
            return True
        else:
            return False
            
    def export(self, f):
        return None
        
    def exportNavmesh(self, sPath):
        print("exportNavmesh 2")
        import bmesh
        if len(self.m_objects) > 0:
            print("exportNavmesh 3")
            with open(sPath+"/navmesh.xml", "w") as navmeshfile:
                navmesh_obj = self.m_objects[0]
                bm = bmesh.new()
                bm.from_mesh(navmesh_obj.data)
                om = navmesh_obj.matrix_world
                
                navmeshfile.write('<?xml version="1.0"?>')
                navmeshfile.write('<navmesh>\n')
                min_height_testing = getObjectProperty(navmesh_obj, "min_height_testing", -1.0)
                max_height_testing = getObjectProperty(navmesh_obj, "max_height_testing", 5.0)
                navmeshfile.write('<height-testing min="%f" max="%f"/>\n' % (min_height_testing, max_height_testing))
                navmeshfile.write('<MaxVertsPerPoly nvp="4" />\n')
                navmeshfile.write('<vertices>\n')
                
                for vert in bm.verts:
                    navmeshfile.write('<vertex x="%f" y="%f" z="%f" />\n' % ((om*vert.co).x, (om*vert.co).z, (om*vert.co).y))
                
                navmeshfile.write('</vertices>\n')
                navmeshfile.write('<faces>\n')
                
                for face in bm.faces:
                    navmeshfile.write('<face indices="')
                    if len(face.verts) != 4:
                        log_error('Use only quad for navmesh, face %d not quad!' % face.index)
                        log_error('To find it out, select the navmesh object and toggle edit mode, than in python console:')
                        log_error('me = bpy.data.objects[\'%s\'].data' % self.m_objects[0].name)
                        log_error('import bmesh')
                        log_error('bm = bmesh.from_edit_mesh(me)')
                        log_error('bm.faces[%d].select = True' % face.index)
                        log_error('bmesh.update_edit_mesh(me, True)')
                        assert False
                    for vert in face.verts:
                        navmeshfile.write('%d ' % vert.index)
                    
                    list_face = []
                    unique_face = []
                    for edge in face.edges:
                        for l_face in edge.link_faces:
                            list_face.append(l_face.index)
                    
                    [unique_face.append(item) for item in list_face if item not in unique_face]
                    unique_face.remove(face.index) #remove current face index
                    
                    navmeshfile.write('" adjacents="')
                    for num in unique_face:
                        navmeshfile.write('%d ' % num)
                    navmeshfile.write('" />\n')
                
                navmeshfile.write('</faces>\n')
                navmeshfile.write('</navmesh>\n')
                
# ------------------------------------------------------------------------------
class DrivelineExporter:
    
    def __init__(self):
        self.lChecks = []
        self.lCannons = []
        self.lDrivelines = []
        self.found_main_driveline = False
        self.lEndCameras = []
    
    def processObject(self, obj, stktype):
        
        if stktype=="CHECK" or stktype=="LAP" or stktype=="GOAL":
            self.lChecks.append(obj)
            return True
        if stktype=="CANNONSTART":
            self.lCannons.append(obj)
            return True
        # Check for new drivelines
        elif stktype=="MAIN-DRIVELINE" or \
                stktype=="MAINDRIVELINE"  or \
                stktype=="MAINDL":
            # Main driveline must be the first entry in the list
            self.lDrivelines.insert(0, Driveline(obj, 1))
            self.found_main_driveline = True
            return True
        elif stktype=="DRIVELINE":
            self.lDrivelines.append(Driveline(obj, 0))
            return True
        elif obj.type=="CAMERA" and stktype in ['FIXED', 'AHEAD']:
            self.lEndCameras.append(obj)
            return True
            
        return False
            
    def export(self, f):
        is_arena = getSceneProperty(bpy.data.scenes[0], "arena", "false") == "true"
        is_soccer = getSceneProperty(bpy.data.scenes[0], "soccer", "false") == "true"
        is_cutscene = getSceneProperty(bpy.data.scenes[0], "cutscene",  "false") == "true"
        if not self.found_main_driveline and not is_arena and not is_soccer and not is_cutscene:
            if len(self.lDrivelines) > 0:
                log_warning("Main driveline missing, using first driveline as main!")
            elif getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true':
                log_error("No driveline found")
        
        if len(self.lDrivelines) == 0:
            self.lDrivelines=[None]
        
        mainDriveline = self.lDrivelines[0]
        if mainDriveline is None and getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true' and not (is_arena or is_soccer):
            log_error("No main driveline found")
        
        self.lChecks = self.lChecks + self.lCannons # cannons at the end, see #1386
        
        if self.lChecks or mainDriveline:
            if not self.lChecks:
                log_warning("No check defined, lap counting will not work properly!")
            self.writeChecks(f, self.lChecks, mainDriveline)
            
        if self.lEndCameras:
            f.write("  <end-cameras>\n")
            for i in self.lEndCameras:
                type = getObjectProperty(i, "type", "ahead").lower()
                if type=="ahead":
                    type="ahead_of_kart"
                elif type=="fixed":
                    type="static_follow_kart"
                else:
                    log_warning ("Unknown camera type %s - ignored." % type)
                    continue
                xyz = "%f %f %f" % (i.location[0], i.location[2], i.location[1])
                start = getObjectProperty(i, "start", 5)
                f.write("    <camera type=\"%s\" xyz=\"%s\" distance=\"%s\"/> <!-- %s -->\n"%
                        (type, xyz, start, i.name) )
            f.write("  </end-cameras>\n")


    # --------------------------------------------------------------------------
    # Finds the closest driveline from the list lDrivelines to the point p (i.e.
    # the driveline for which the distance between p and the drivelines start
    # point is as small as possible. Returns the index of the closest drivelines.
    def findClosestDrivelineToPoint(self, lDrivelines, p):
        min_index = 0
        min_dist  = lDrivelines[0].getStartDistanceTo(p)
        for i in range(1,len(lDrivelines)):
            driveline=lDrivelines[i]
            dist_new = driveline.getStartDistanceTo(p)
            if dist_new<min_dist:
                min_dist  = dist_new
                min_index = i

        return min_index
    
    # --------------------------------------------------------------------------
    # Find the driveline from lRemain that is closest to any of the drivelines
    # in lSorted.
    def findClosestDrivelineToDrivelines(self, lRemain, lSorted):
        remain_index                    = 0
        (min_dist, sorted_index, min_quad) = lRemain[0].getDistanceToStart(lSorted)
        for i in range(1, len(lRemain)):
            (dist, index, quad) = lRemain[i].getDistanceToStart(lSorted)
            if dist<min_dist:
                min_dist     = dist
                sorted_index = index
                min_quad     = quad
                remain_index = i
        return (remain_index, sorted_index, min_quad)
        
    # --------------------------------------------------------------------------
    # Converts a new drivelines. New drivelines have the following structure:
    #   +---+---+--+--...--+--
    #   |   |      |       |  
    #   +---+--+---+--...--+--
    # The starting quad of the drivelines is marked by two edges ending in a
    # single otherwise unconnected vertex. These two vertices (and edges) are
    # not used in the actual driveline, they are only used to indicate where
    # the drivelines starts. This data structure is handled in the Driveline
    # class.
    # Additionally, this function sorts the end cameras according to distance
    # to the main driveline - so the first end camera will be the camera
    # closest to the start line etc.
    def convertDrivelinesAndSortEndCameras(self, lDrivelines, lSorted,
                                           lEndCameras):
        # First collect all main drivelines, and all remaining drivelines
        # ---------------------------------------------------------------
        lMain     = []
        lRemain   = []
        for driveline in lDrivelines:
            if driveline.isMain():
                lMain.append(driveline)
            else:
                lRemain.append(driveline)

        # Now collect all main drivelines in one list starting
        # with the closest to 0, then the one closest to the
        # end of the first one, etc
        p          = (0,0,0)
        quad_index = 0
        while lMain:
            min_index = self.findClosestDrivelineToPoint(lMain, p)
            # Move the main driveline with minimal distance to the
            # sorted list.
            lSorted.append(lMain[min_index])
            del lMain[min_index]
            
            # Set the start quad index for all quads.
            lSorted[-1].setStartQuadIndex(quad_index)
            quad_index = quad_index + lSorted[-1].getNumberOfQuads()

            p = lSorted[-1].getEndPoint()

        # Create a new list for all cameras, which also stores the
        # quad index to which the camera is closest to, the distance
        # to the quad, and the camera object. The order is important
        # since this list is later sorted by quad index, so that the
        # first camera is the first in the list.
        lCamerasDistance = []
        for i in range(len(lEndCameras)):
            cam = lEndCameras[i]
            try:
                (distance, driveline_index, quad_index_camera) = \
                           lSorted[0].getDistanceTo(cam.location, lSorted)
                # Each list contains the index of the closest quad, the
                # distance, and then the camera
                lEndCameras[i] = (driveline_index, quad_index_camera, cam)
            except:
                log_warning("Problem with the end camera '%s'. Check if the main driveline is " +\
                            "properly defined (check warning messages), and the " +\
                            "settings of the camera."%cam.name)
                
        lEndCameras.sort()
        
        # After sorting remove the unnecessary distance and quad index
        for i in range(len(lEndCameras)):
            # Avoid crash in case that some problem with the camera happened,
            # and lEndCameras is just the blender camera, not the tuple
            if type(lEndCameras[i])==type(()):
                lEndCameras[i] = lEndCameras[i][2]

        # There were already two warning messages printed at this stage, so just
        # ignore this to avoid further crashes
        if len(lSorted) < 1:
            return
        
        # The last main driveline needs to be closed to the first quad.
        # So set a flag in that driveline that it is the last one.
        lSorted[-1].setIsLastMain(lSorted[0])
        quad_index = quad_index + 1
        
        # Now add the remaining drivelines one at a time. From all remaining
        # drivelines we pick the one closest to the drivelines contained in
        # lSorted.
        while lRemain:
            t = self.findClosestDrivelineToDrivelines(lRemain, lSorted)
            (remain_index, sorted_index, quad_to_index) = t
            lRemain[remain_index].setFromQuad(lSorted[sorted_index],
                                              quad_to_index)
            lSorted.append(lRemain[remain_index])
            del lRemain[remain_index]

            # Set the start quad index for all quads.
            lSorted[-1].setStartQuadIndex(quad_index)
            quad_index = quad_index + lSorted[-1].getNumberOfQuads()

    # --------------------------------------------------------------------------
    # Writes the track.quad file with the list of all quads, and the track.graph
    # file defining a graph node for each quad and a basic connection between
    # all graph nodes.
    def writeQuadAndGraph(self, sPath):
        #start_time = bsys.time()
        
        lDrivelines = self.lDrivelines
        lEndCameras = self.lEndCameras
        
        print("Writing quad file --> \t")
        if not lDrivelines:
            print("No main driveline defined, no driveline information exported!!!")
            return
    
        lSorted = []
        self.convertDrivelinesAndSortEndCameras(lDrivelines, lSorted, lEndCameras)

        # That means that there were some problems with the drivelines, and
        # it doesn't make any sense to continue anyway
        if not lSorted:
            return
        
        # Stores the first quad number (and since quads = graph nodes the node
        # number) of each section of the track. I.e. the main track starts with
        # quad 0, then the first alternative way, ...
        lStartQuad         = [0]
        dSuccessor         = {}
        last_main_lap_quad = 0
        count              = 0
        
        f = open(sPath+"/quads.xml", "w")
        f.write("<?xml version=\"1.0\"?>\n")
        f.write("<!-- Generated with script from SVN rev %s -->\n"%getScriptVersion())
        f.write("<quads>\n")
        f.write('  <height-testing min="%f" max="%f"/>\n' %\
        (lSorted[0].min_height_testing, lSorted[0].max_height_testing))

        for driveline in lSorted:
            driveline.writeQuads(f)

        f.write("</quads>\n")
        f.close()
        #print bsys.time() - start_time,"seconds. "

        #start_time = bsys.time()
        print("Writing graph file --> \t")
        f=open(sPath+"/graph.xml", "w")
        f.write("<?xml version=\"1.0\"?>\n")
        f.write("<!-- Generated with script from SVN rev %s -->\n"%getScriptVersion())
        f.write("<graph>\n")
        f.write("  <!-- First define all nodes of the graph, and what quads they represent -->\n")
        f.write("  <node-list from-quad=\"%d\" to-quad=\"%d\"/>  <!-- map each quad to a node  -->\n"\
                %(0, lSorted[-1].getLastQuadIndex()))

        f.write("  <!-- Define the main loop -->\n");
        last_main = None
        for i in lSorted:
            if i.isMain():
                last_main = i
            else:
                break

        # The main driveline is written as a simple loop
        f.write("  <edge-loop from=\"%d\" to=\"%d\"/>\n" %
                (0, last_main.getLastQuadIndex()) )

        # Each non-main driveline writes potentially three entries in the
        # graph file: connection to the beginning of this driveline, the
        # driveline quads themselves, and a connection from the end of the
        # driveline to another driveline. But this can result in edged being
        # written more than once: consider two non-main drivelines A and B
        # which are connected to each other. Then A will write the edge from
        # A to B as its end connection, and B will write the same connection
        # as its begin connection. To avoid this, we keep track of all
        # written from/to edges, and only write one if it hasn't been written.
        dWrittenEdges={}
        # Now write the remaining drivelines
        for driveline in lSorted:
            # Mainline was already written, so ignore it
            if driveline.isMain(): continue

            f.write("  <!-- Shortcut %s -->\n"%driveline.getName())
            # Write the connection from an already written quad to this
            fr = driveline.getFromQuad()
            to = driveline.getFirstQuadIndex()
            if (fr,to) not in dWrittenEdges:
                f.write("  <edge from=\"%d\" to=\"%d\"/>\n" %(fr, to))
                #if to.isEnabled() and fr.isEnabled():
                #    f.write("  <edge from=\"%d\" to=\"%d\"/>\n" %(fr, to))
                #elif to.isEnabled():
                #    f.write("  <!-- %s disabled <edge from=\"%d\" to=\"%d\"/> -->\n" \
                #            %(fr.getName(), fr, to))
                #else:
                #    f.write("  <!-- %s disabled <edge from=\"%d\" to=\"%d\"/> -->\n"
                #            %(to.getName(), fr, to))
                dWrittenEdges[ (fr, to) ] = 1
            if driveline.getFirstQuadIndex()< driveline.getLastQuadIndex():
                f.write("  <edge-line from=\"%d\" to=\"%d\"/>\n" \
                        %(driveline.getFirstQuadIndex(),
                          driveline.getLastQuadIndex()))
            fr = driveline.getLastQuadIndex()
            to = driveline.computeSuccessor(lSorted)
            if (fr, to) not in dWrittenEdges:
                f.write("  <edge from=\"%d\" to=\"%d\"/>\n" %(fr, to))
                dWrittenEdges[ (fr, to) ] = 1
        f.write("</graph>\n")
        f.close()
        #print bsys.time()-start_time,"seconds. "
          
    # --------------------------------------------------------------------------
    # Writes out all checklines.
    # \param lChecks All check meshes
    # \param mainDriveline The main driveline, from which the lap
    #        counting check line is determined.
    def writeChecks(self, f, lChecks, mainDriveline):
        f.write("  <checks>\n")
        
        # A dictionary containing a list of indices of check structures
        # that belong to this group.
        dGroup2Indices = {"lap":[0]}
        # Collect the indices of all check structures for all groups
        ind = 1
        for obj in lChecks:
            name = getObjectProperty(obj, "type", obj.name.lower()).lower()
            if len(name) == 0: name = obj.name.lower()
            
            type = getObjectProperty(obj, "type", "")
            if type == "cannonstart" or type == "cannonend":
                continue
                
            if name!="lap":
                name = getObjectProperty(obj, "name", obj.name.lower()).lower()
            if name in dGroup2Indices:
                dGroup2Indices[name].append(ind)
            else:
                dGroup2Indices[name] = [ ind ]
            ind = ind + 1
            
        print("**** dGroup2Indices:", dGroup2Indices)

        if mainDriveline:
            lap = mainDriveline.getStartEdge()
            
            strict_lapline = mainDriveline.isStrictLapline()
            
            if lap[0] is None:
                return # Invalid driveline (a message will have been printed)
            
            coord = lap[0]
            min_h = coord[2]
            if coord[2] < min_h: min_h = coord[2]

            # The main driveline is always the first entry, so remove
            # only the first entry to get the list of all other lap lines
            l = dGroup2Indices["lap"]
            
            from functools import reduce
            sSameGroup = reduce(lambda x,y: str(x)+" "+str(y), l, "")
            
            activate = mainDriveline.getActivate()
            if activate:
                group = activate.lower()
                
                if not group or group not in dGroup2Indices:
                    log_warning("Activate group '%s' not found!"%group)
                    print("Ignored - but lap counting might not work correctly.")
                    print("Make sure there is an object of type 'check' with")
                    print("the name '%s' defined."%group)
                    activate = ""
                else:
                    activate = reduce(lambda x,y: str(x)+" "+str(y), dGroup2Indices[group])
            else:
                group = ""
                activate = ""
                log_warning("Warning : the main driveline does not activate any checkline. Lap counting and kart rescue will not work correctly.")
        else:
            # No main drive defined, print a warning and add some dummy
            # driveline (makes the rest of this code easier)
            lap        = [ [-1, 0], [1, 0] ]
            min_h      = 0
            sSameGroup = ""
            activate = ""
            strict_lapline = True

        if sSameGroup:
            sSameGroup="same-group=\"%s\""%sSameGroup.strip()

        if activate:
            activate = "other-ids=\"%s\""%activate
        
        if not strict_lapline:
            f.write("    <check-lap kind=\"lap\" %s %s />\n"%(sSameGroup, activate))
        else:
            f.write("    <check-line kind=\"lap\" p1=\"%.2f %.2f\" p2=\"%.2f %.2f\"\n"% \
                    (lap[0][0], lap[0][1],
                     lap[1][0], lap[1][1] )  )
            f.write("                min-height=\"%.2f\" %s %s/>\n"% (min_h, sSameGroup, activate) )

        ind = 1
        for obj in lChecks:
        
            try:
                type = getObjectProperty(obj, "type", "")
                if type == "cannonstart":
                    self.writeCannon(f, obj)
                    continue
                elif type == "cannonend":
                    continue
                elif type == "goal":
                    self.writeGoal(f, obj)
                    continue
                
                mesh = obj.data.copy()
                # Convert to world space
                mesh.transform(obj.matrix_world)
                # One of lap, activate, toggle, ambient
                activate = getObjectProperty(obj, "activate", "")
                kind=" "
                if activate:
                    group = activate.lower()
                    if group not in dGroup2Indices:
                        log_warning("Activate group '%s' not found!"%group)
                        print("Ignored - but lap counting might not work correctly.")
                        print("Make sure there is an object of type 'check' with")
                        print("the name '%s' defined."%group)
                        continue
                    s = reduce(lambda x,y: str(x)+" "+str(y), dGroup2Indices[group])
                    kind = " kind=\"activate\" other-ids=\"%s\" "% s

                toggle = getObjectProperty(obj, "toggle", "")
                if toggle:
                    group = toggle.lower()
                    if group not in dGroup2Indices:
                        log_warning("Toggle group '%s' not found!"%group)
                        print("Ignored - but lap counting might not work correctly.")
                        print("Make sure there is an object of type 'check' with")
                        print("the name '%s' defined."%group)
                        continue
                    s = reduce(lambda x,y: str(x)+" "+str(y), dGroup2Indices[group])
                    kind = " kind=\"toggle\" other-ids=\"%s\" "% s

                lap = getObjectProperty(obj, "type", obj.name).upper()
                if lap[:3]=="LAP":
                    kind = " kind=\"lap\" "  # xml needs a value for an attribute
                    activate = getObjectProperty(obj, "activate", "")
                    if activate:
                        group = activate.lower()
                        if group not in dGroup2Indices:
                            log_warning("Activate group '%s' not found for lap line!"%group)
                            print("Ignored - but lap counting might not work correctly.")
                            print("Make sure there is an object of type 'check' with")
                            print("the name '%s' defined."%group)
                            continue
                        s = reduce(lambda x,y: str(x)+" "+str(y), dGroup2Indices[group])
                        kind = "%sother-ids=\"%s\" "% (kind, s)
                
                ambient = getObjectProperty(obj, "ambient", "").upper()
                if ambient:
                    kind=" kind=\"ambient-light\" "

                # Get the group name this object belongs to. If the objects
                # is of type lap then 'lap' is the group name, otherwise
                # it's taken from the name property (or the object name).
                name = getObjectProperty(obj, "type", obj.name.lower()).lower()
                if name!="lap":
                    name = getObjectProperty(obj, "name", obj.name.lower()).lower()
                    if len(name) == 0: name = obj.name.lower()
                    
                # Get the list of indices of this group, excluding
                # the index of the current object. So create a copy
                # of the list and remove the current index
                l = dGroup2Indices[name][:]
                sSameGroup = reduce(lambda x,y: str(x)+" "+str(y), l, "")
                ind = ind + 1

                if len(mesh.vertices)==2:   # Check line
                    min_h = mesh.vertices[0].co[2]
                    if mesh.vertices[1].co[2] < min_h: min_h = mesh.vertices[1].co[2]
                    f.write("    <check-line%sp1=\"%.2f %.2f\" p2=\"%.2f %.2f\"\n" %
                            (kind, mesh.vertices[0].co[0], mesh.vertices[0].co[1],
                             mesh.vertices[1].co[0], mesh.vertices[1].co[1]   )  )

                    f.write("                min-height=\"%.2f\" same-group=\"%s\"/>\n" \
                            % (min_h, sSameGroup.strip())  )
                else:
                    radius = 0
                    for v in mesh.vertices:
                        r = (obj.location[0]-v[0])*(obj.location[0]-v[0]) + \
                            (obj.location[1]-v[1])*(obj.location[1]-v[1]) + \
                            (obj.location[2]-v[2])*(obj.loc[2]-v[2])
                        if r > radius:
                            radius = r
                    
                    radius = math.sqrt(radius)
                    inner_radius = getObjectProperty(obj, "inner_radius", radius)
                    color = getObjectProperty(obj, "color", "255 120 120 120")
                    f.write("    <check-sphere%sxyz=\"%.2f %.2f %.2f\" radius=\"%.2f\"\n" % \
                            (kind, obj.location[0], obj.location[2], obj.location[1], radius) )
                    f.write("                  same-group=\"%s\"\n"%sSameGroup.strip())
                    f.write("                  inner-radius=\"%.2f\" color=\"%s\"/>\n"% \
                            (inner_radius, color) )
            except Exception as exc:
                log_error("Error exporting checkline " + obj.name + ", make sure it is properly formed")
                
                from traceback import format_tb
                print(format_tb(exc.__traceback__)[0])
        f.write("  </checks>\n")
   
    # Write out a goal line
    def writeGoal(self, f, goal):
        if len(goal.data.vertices) != 2:
            log_warning("Goal line is not a line made of 2 vertices as expected")
            
        goal_matrix = goal.rotation_euler.to_matrix()
        
        goal_pt1 = goal.data.vertices[0].co*goal_matrix + goal.location
        goal_pt2 = goal.data.vertices[1].co*goal_matrix + goal.location
        
        first_goal_string = ""
        if getObjectProperty(goal, "first_goal", "false") == "true":
            first_goal_string=" first_goal=\"true\" "
        
        f.write('    <goal p1="%.2f %.2f %.2f" p2="%.2f %.2f %.2f" %s/>\n'%\
                (goal_pt1[0], goal_pt1[2], goal_pt1[1],
                 goal_pt2[0], goal_pt2[2], goal_pt2[1],
                 first_goal_string))
    
    # Writes out all cannon checklines.
    def writeCannon(self, f, cannon):
    
        start = cannon
        
        endSegmentName = getObjectProperty(start, "cannonend", "")
        if len(endSegmentName) == 0 or endSegmentName not in bpy.data.objects:
            log_error("Cannon " + cannon.name + " end is not defined")
            return
        
        end = bpy.data.objects[endSegmentName]
        
        if len(start.data.vertices) != 2:
            log_warning("Cannon start " + start.name + " is not a line made of 2 vertices as expected")
        if len(end.data.vertices) != 2:
            log_warning("Cannon end " + end.name + " is not a line made of 2 vertices as expected")

        curvename = getObjectProperty(start, "cannonpath", "")
        start_pt1 = start.matrix_world * start.data.vertices[0].co
        start_pt2 = start.matrix_world * start.data.vertices[1].co
        end_pt1 = end.matrix_world * end.data.vertices[0].co
        end_pt2 = end.matrix_world * end.data.vertices[1].co

        f.write('    <cannon p1="%.2f %.2f %.2f" p2="%.2f %.2f %.2f" target-p1="%.2f %.2f %.2f" target-p2="%.2f %.2f %.2f">\n'%\
                (start_pt1[0], start_pt1[2], start_pt1[1],
                 start_pt2[0], start_pt2[2], start_pt2[1],
                 end_pt1[0],   end_pt1[2],   end_pt1[1],
                 end_pt2[0],   end_pt2[2],   end_pt2[1]))

        if len(curvename) > 0:
            writeBezierCurve(f, bpy.data.objects[curvename], \
                             getObjectProperty(start, "cannonspeed", 50.0), "const" )
        
        f.write('    </cannon>\n')
        
# ==============================================================================
# A special class to store a drivelines.
class Driveline:
    def __init__(self, driveline, is_main):
        self.name      = driveline.name
        self.is_main   = is_main
        # Transform the mesh to the right coordinates.
        self.mesh      = driveline.data.copy()
        self.mesh.transform(driveline.matrix_world)
        # Convert the mesh into a dictionary: each vertex is a key to a
        # list of neighbours.
        self.createNeighbourDict()
        self.defineStartVertex()
        self.convertToLists()
        self.from_quad=None
        self.from_driveline=None
        self.to_driveline=None
        self.is_last_main = 0
        # Invisible drivelines are not shown in the minimap
        self.invisible = getObjectProperty(driveline, "invisible", "false")
        self.ai_ignore = getObjectProperty(driveline, "ai_ignore", "false")
        self.direction = getObjectProperty(driveline, "direction", "both")
        self.enabled   = not getObjectProperty(driveline, "disable",   0)
        self.activate  = getObjectProperty(driveline, "activate", None)
        self.strict_lap = convertTextToYN(getObjectProperty(driveline,
                                                      "strict_lapline", "N") ) \
                           == "Y"
        self.min_height_testing = getObjectProperty(driveline, "min_height_testing", -1.0)
        self.max_height_testing = getObjectProperty(driveline, "max_height_testing", 5.0)

    # --------------------------------------------------------------------------
    # Returns the name of the driveline
    def getName(self):
        return self.name
    # --------------------------------------------------------------------------
    # Returns if this is a main driveline or not.
    def isMain(self):
        return self.is_main
    # --------------------------------------------------------------------------
    # Returns if this driveline is disabled.
    def isEnabled(self): 
        return self.enabled
    # --------------------------------------------------------------------------
    # Returns the 'activate' property of the driveline object.
    def getActivate(self):
        return self.activate
    # --------------------------------------------------------------------------
    # Returns if this driveline requests strict lap counting (i.e. exactly
    # crossing the line between the start vertices)
    def isStrictLapline(self):
        return self.strict_lap
    # --------------------------------------------------------------------------
    # Stores that the start quad of this driveline is connected to quad
    # quad_index of quad driveline. 
    def setFromQuad(self, driveline, quad_index):
        # Convert the relative to driveline quad index to the global index:
        self.from_quad      = driveline.getFirstQuadIndex()+quad_index
        self.from_driveline = driveline
    # --------------------------------------------------------------------------
    def setToDriveline(self, driveline):
        self.to_driveline = driveline
    # --------------------------------------------------------------------------
    # Returns the global index of the quad this start point is connected to.
    def getFromQuad(self):
        return self.from_quad
    # --------------------------------------------------------------------------
    # Returns the number of quads of this driveline
    def getNumberOfQuads(self):
        return len(self.lCenter)
    # --------------------------------------------------------------------------
    # Stores the index of the first quad in this driveline in the global
    # quad index.
    def setStartQuadIndex(self, n):
        self.global_quad_index_start = n
    # --------------------------------------------------------------------------
    # Returns the start index for this driveline in the global numbering of
    # all quads
    def getFirstQuadIndex(self):
        return self.global_quad_index_start
    # --------------------------------------------------------------------------
    # Returns the global index of the last quad in this driveline.
    def getLastQuadIndex(self):
        return self.global_quad_index_start+len(self.lCenter)-1
    # --------------------------------------------------------------------------
    # Returns the start edge, which is the lap counting line for the main
    # drivelines. See defineStartVertex() for setting self.start_line.
    def getStartEdge(self):
        return self.start_line
    # --------------------------------------------------------------------------
    # This driveline is the last main driveline. This means that it will get
    # one additional quad added to connect this to the very first quad. Since
    # the values are not actually needed (see write function), the arrays have
    # to be made one element larger to account for this additional quad (e.g.
    # in calls to getNumberOfQuads etc).
    def setIsLastMain(self, first_driveline):
        self.is_last_main = 1
        cp=[]
        
        for i in range(3):
          
            if self.lRight[-1] is None or self.lLeft[-1] is None:
                return # Invalid driveline (an error message will have been printed)
            
            cp.append((self.mesh.vertices[self.lLeft[-1]].co[i] + 
                       first_driveline.mesh.vertices[first_driveline.lLeft[0]].co[i]+
                       self.mesh.vertices[self.lRight[-1]].co[i] +
                       first_driveline.mesh.vertices[first_driveline.lRight[0]].co[i])*0.25)

        self.lCenter.append(cp)
        self.lLeft.append(None)
        self.lRight.append(None)

    # --------------------------------------------------------------------------
    # This creates a dictionary for a mesh which contains for each vertex a list
    # of all its neighbours.
    def createNeighbourDict(self):
        self.dNext = {}
        for e in self.mesh.edges:
            if e.vertices[0] in self.dNext:
                self.dNext[e.vertices[0]].append(e.vertices[1])
            else:
                self.dNext[e.vertices[0]] = [e.vertices[1]]
            
            if e.vertices[1] in self.dNext:
                self.dNext[e.vertices[1]].append(e.vertices[0])
            else:
                self.dNext[e.vertices[1]] = [e.vertices[0]]

    # --------------------------------------------------------------------------
    # This helper function determines the start vertex for a driveline.
    # Details are documented in convertDrivelines. It returns as list with
    # the two starting lines.
    def defineStartVertex(self):
        # Find all vertices with exactly two neighbours
        self.lStart = []
        for i in self.dNext.keys():
            if len(self.dNext[i])==1:
                self.lStart.append( i )

        if len(self.lStart)!=2:
            log_error("Driveline '%s' is incorrectly formed, cannot find the two 'antennas' that indicate where the driveline starts." % self.name)
            self.start_point = (0,0,0)
            return

        print("self.lStart[0] =", self.lStart[0])
        print("self.lStart[1] =", self.lStart[1])

        start_coord_1 = self.mesh.vertices[self.lStart[0]].co
        start_coord_2 = self.mesh.vertices[self.lStart[1]].co

        # Save the middle of the first quad, which is used later for neareast
        # quads computations.
        self.start_point = ((start_coord_1[0] + start_coord_2[0])*0.5,
                            (start_coord_1[1] + start_coord_2[1])*0.5,
                            (start_coord_1[2] + start_coord_2[2])*0.5 )

    # --------------------------------------------------------------------------
    # Returns the startline of this driveline
    def getStartPoint(self):
        return self.start_point
    # --------------------------------------------------------------------------
    # Returns the distance of the start point from a given point
    def getStartDistanceTo(self, p):
        dx=self.start_point[0]-p[0]
        dy=self.start_point[1]-p[1]
        dz=self.start_point[2]-p[2]
        return dx*dx+dy*dy+dz*dz
    # --------------------------------------------------------------------------
    # Convert the dictionary of list of neighbours to two lists - one for the
    # left side, one for the right side.
    def convertToLists(self):
      
        if len(self.lStart) < 2:
            self.lLeft = [None, None]
            self.lRight = [None, None]
            self.start_line = (None, None)
            self.end_point = (0,0,0)
            self.lCenter = []
            return
      
        self.lLeft   = [self.lStart[0], self.dNext[self.lStart[0]][0]]
        self.lRight  = [self.lStart[1], self.dNext[self.lStart[1]][0]]
        self.lCenter = []
        
        # this is for error handling only
        processed_vertices = {}
        processed_vertices[self.lStart[0]] = True
        processed_vertices[self.lStart[1]] = True

        # The quads can be either clockwise or counter-clockwise oriented. STK
        # expectes counter-clockwise, so if the orientation is wrong, swap
        # left and right side.
        
        left_0_coord = self.mesh.vertices[self.lLeft[0]].co
        #left_1_coord = self.mesh.vertices[self.lLeft[1]].co
        right_0_coord = self.mesh.vertices[self.lRight[0]].co
        right_1_coord = self.mesh.vertices[self.lRight[1]].co
        
        if (right_1_coord[0] - left_0_coord[0])*(right_0_coord[1] - left_0_coord[1]) \
         - (right_1_coord[1] - left_0_coord[1])*(right_0_coord[0] - left_0_coord[0]) > 0:
            r   = self.lRight
            self.lRight = self.lLeft
            self.lLeft  = r
        
        # Save start edge, which will become the main lap counting line
        # (on the main driveline). This must be done here after potentially 
        # switching since STK assumes that the first point of a check line (to 
        # which the first line of the main driveline is converted) is on the 
        # left side (this only applies for the lap counting line, see
        # Track::setStartCoordinates/getStartTransform).
        self.start_line = (self.mesh.vertices[self.lLeft[1]].co, self.mesh.vertices[self.lRight[1]].co)
        
        count=0
        # Just in case that we have an infinite loop due to a malformed graph:
        # stop after 10000 vertices
        max_count = 10000
        warning_printed = 0

        while count < max_count:
            count = count + 1

            processed_vertices[self.lLeft[-1]] = True

            # Get all neighbours. One is the previous point, one
            # points to the opposite side - we need the other one.
            neighb = self.dNext[self.lLeft[-1]]
            next_left = []
            for i in neighb:
                if i==self.lLeft[-2]: continue   # pointing backwards
                if i==self.lRight[-1]: continue  # to opposite side
                next_left.append(i)
            
            if len(next_left) == 0:
                # No new element found --> this must be the end
                # of the list!!
                break
            
            if len(next_left)!=1 and not warning_printed:
                lcoord = self.mesh.vertices[self.lLeft[-1]].co
                rcoord = self.mesh.vertices[self.lRight[-1]].co
                log_warning("Broken driveline at or around point ({0}, {1}, {2})".format\
                            (lcoord[0], lcoord[1], lcoord[2]))
                print("Potential successors :")
                for i in range(len(next_left)):
                    nextco = self.mesh.vertices[next_left[i]].co
                    print ("Successor %d: %f %f %f" % \
                          (i, nextco[0], nextco[1], nextco[2]))
                print ("It might also possible that the corresponding right driveline point")
                print (rcoord[0],rcoord[1],rcoord[2])
                print ("has some inconsistencies.")
                print ("The drivelines will most certainly not be useable.")
                print ("Further warnings are likely and will be suppressed.")
                warning_printed = 1
                operator.report({'ERROR'}, "Problems with driveline detected, check console for details!")
                # Blender.Draw.PupMenu("Problems with driveline detected, check console for details!")
                
            self.lLeft.append(next_left[0])

            
            processed_vertices[self.lRight[-1]] = True

            # Same for other side:
            neighb = self.dNext[self.lRight[-1]]
            next_right = []
            
            for i in neighb:
                if i==self.lRight[-2]: continue   # pointing backwards
                # Note lLeft has already a new element appended,
                # so we have to check for the 2nd last element!
                if i==self.lLeft[-2]: continue  # to opposite side
                next_right.append(i)
            
            if len(next_right)==0:
                lcoord = self.mesh.vertices[self.lLeft[-1]].co
                rcoord = self.mesh.vertices[self.lRight[-1]].co
                log_warning("Malformed driveline at or around points ({0}, {1}, {2}) and ({3}, {4}, {5})".format\
                             (lcoord[0],lcoord[1],lcoord[2],
                              rcoord[0],rcoord[1],rcoord[2]))
                print ("No more vertices on right side of quad line, but there are")
                print ("still points on the left side. Check the points:")
                print ("left: ", lcoord[0],lcoord[1],lcoord[2])
                print ("right: ", rcoord[0],rcoord[1],rcoord[2])
                print ("Last left point is ignored.")
                break
            
            if len(next_right)!=1 and not warning_printed:
                lcoord = self.mesh.vertices[self.lLeft[-1]].co
                rcoord = self.mesh.vertices[self.lRight[-1]].co
                
                log_error("Invalid driveline at or around point ({0}, {1}, {2})".format\
                          (rcoord[0],rcoord[1],rcoord[2]))
                print ("Warning: More than one potential succesor found for right driveline point")
                print (rcoord[0],rcoord[1],rcoord[2],":")
                #for i in range(len(next_right)):
                #    print ("Successor %d: %f %f %f" % \
                #          (i,next_right[i][0],next_right[i][1],next_right[i][2]))
                print ("It might also possible that the corresponding left driveline point")
                print (lcoord[0],lcoord[1],lcoord[2])
                print ("has some inconsistencies.")
                print ("The drivelines will most certainly not be useable.")
                print ("Further warnings are likely and will be suppressed.")
                warning_printed = 1
                operator.report({'ERROR'}, "Problems with driveline detected!")
                break                
            self.lRight.append(next_right[0])

            processed_vertices[self.lRight[-1]] = True
            processed_vertices[self.lLeft[-1]] = True
            processed_vertices[self.lRight[-2]] = True
            processed_vertices[self.lLeft[-2]] = True

            cp=[]
            for i in range(3):
                cp.append((self.mesh.vertices[self.lLeft[-2]].co[i] + 
                           self.mesh.vertices[self.lLeft[-1]].co[i] +
                           self.mesh.vertices[self.lRight[-2]].co[i] +
                           self.mesh.vertices[self.lRight[-1]].co[i])*0.25)
            self.lCenter.append(cp)

        if count>=max_count and not warning_printed:
            log_warning("Warning, Only the first %d vertices of driveline '%s' are exported" %\
                        (max_count, self.name))
        
        if warning_printed != 1:
            
            not_connected = None
            not_connected_distance = 99999
            
            for v in self.dNext:
                if not v in processed_vertices:
                    
                    # find closest connected vertex (this is only to improve the error message)
                    for pv in processed_vertices:
                        dist = (self.mesh.vertices[v].co - self.mesh.vertices[pv].co).length
                        if dist < not_connected_distance:
                            not_connected_distance = dist
                            not_connected = v
            
            if not_connected:
                log_warning("Warning, driveline '%s' appears to be broken in separate sections. Vertex at %f %f %f is not connected with the rest" % \
                                    (self.name,
                                     self.mesh.vertices[not_connected].co[0],
                                     self.mesh.vertices[not_connected].co[1],
                                     self.mesh.vertices[not_connected].co[2]))

        
        # Now remove the first two points, which are only used to indicate
        # the starting point:
        del self.lLeft[0]
        del self.lRight[0]
        self.end_point =((self.mesh.vertices[self.lLeft[-1]].co[0] +
                          self.mesh.vertices[self.lRight[-1]].co[0])*0.5,
                         (self.mesh.vertices[self.lLeft[-1]].co[1] +
                          self.mesh.vertices[self.lRight[-1]].co[1])*0.5,
                         (self.mesh.vertices[self.lLeft[-1]].co[2] +
                          self.mesh.vertices[self.lRight[-1]].co[2])*0.5 )

    # --------------------------------------------------------------------------
    # Returns the end point of this driveline
    def getEndPoint(self):
        return self.end_point
    
    # --------------------------------------------------------------------------
    def getDistanceToStart(self, lDrivelines):
        return self.getDistanceTo(self.start_point, lDrivelines)
    
    # --------------------------------------------------------------------------
    # Returns the shortest distance to any of the drivelines in the list
    # lDrivelines from the given point p (it's actually a static function).
    # The distance is defined to be the shortest distance from the
    # start point of this driveline to all quads of all drivelines in
    # lDrivelines. This function returns the distance, the index of the
    # driveline in lDrivelines, and the local index of the quad within this
    # driveline as a tuple.
    def getDistanceTo(self, p, lDrivelines):
        if not lDrivelines: return (None, None, None)
        
        (min_dist, min_quad_index) = lDrivelines[0].getMinDistanceToPoint(p)
        min_driveline_index        = 0
        for i in range(1, len(lDrivelines)):
            if lDrivelines[i]==self: continue   # ignore itself
            (dist, quad_index) = lDrivelines[i].getMinDistanceToPoint(p)
            if dist < min_dist:
                min_dist            = dist
                min_quad_index      = quad_index
                min_driveline_index = i
        return (min_dist, min_driveline_index, min_quad_index)

    # --------------------------------------------------------------------------
    # Returns the minimum distance from the center point of each quad to the
    # point p.
    def getMinDistanceToPoint(self, p):
        pCenter   = self.lCenter[0]
        dx        = pCenter[0]-p[0]
        dy        = pCenter[1]-p[1]
        dz        = pCenter[2]-p[2]
        min_dist  = dx*dx+dy*dy+dz*dz
        min_index = 0
        for i in range(1, len(self.lCenter)):
            pCenter = self.lCenter[i]
            dx      = pCenter[0]-p[0]
            dy      = pCenter[1]-p[1]
            dz      = pCenter[2]-p[2]
            d       = dx*dx+dy*dy+dz*dz
            if d<min_dist:
                min_dist  = d
                min_index = i
        return (min_dist, min_index)

    # --------------------------------------------------------------------------
    # Determine the driveline from lSorted which is closest to this driveline's
    # endpoint (closest meaning: having a quad that is closest).
    def computeSuccessor(self, lSorted):
        (dist, driveline_index, quad_index)=self.getDistanceTo(self.end_point,
                                                               lSorted)
        return quad_index + lSorted[driveline_index].getFirstQuadIndex()

    # --------------------------------------------------------------------------
    # Writes the quads into a file.
    def writeQuads(self, f):
      
        if self.lLeft[0] is None or self.lRight[0] is None:
            return # Invalid driveline (a message will have been printed)
        if self.lLeft[1] is None or self.lRight[1] is None:
            return # Invalid driveline (a message will have been printed)
      
        l   = self.mesh.vertices[self.lLeft[0]].co
        r   = self.mesh.vertices[self.lRight[0]].co
        l1  = self.mesh.vertices[self.lLeft[1]].co
        r1  = self.mesh.vertices[self.lRight[1]].co

        if self.invisible and self.invisible=="true":
            sInv = " invisible=\"yes\" "
        else:
            sInv = " "
        
        # AI-ignore will be applied to the first and last quad (to account for forward and reverse mode)
        if self.ai_ignore and self.ai_ignore=="true":
            sAIIgnore = "ai-ignore=\"yes\" "
        else:
            sAIIgnore = " "
        
        if self.direction and self.direction != "both":
            sDirection = "direction=\"" + self.direction + "\" "
        else:
            sDirection = " "
            
        max_index = len(self.lLeft) - 1
        
        # If this is the last main driveline, the last quad is a dummy element
        # added by setLastMain(). So the number of elements is decreased by
        # one.
        if self.is_last_main:
            max_index = max_index - 1
            
        f.write("  <!-- Driveline: %s -->\n"%self.name)
        # Note that only the first quad must be marked with ai-ignore
        # (this results that the AI will not go to the first quad, but
        # if it should end up somewhere on the shortcut, it will
        # continue to drive on the shortcut.
        f.write("  <quad%s%s%sp0=\"%.3f %.3f %.3f\" p1=\"%.3f %.3f %.3f\" p2=\"%.3f %.3f %.3f\" p3=\"%.3f %.3f %.3f\"/>\n" \
            %(sInv, sAIIgnore, sDirection, l[0],l[2],l[1], r[0],r[2],r[1], r1[0],r1[2],r1[1], l1[0],l1[2],l1[1]) )
        for i in range(1, max_index):
            if self.lRight[i+1] is None: return # broken driveline (messages will already have been printed)
          
            l1  = self.mesh.vertices[self.lLeft[i+1]].co
            r1  = self.mesh.vertices[self.lRight[i+1]].co
            f.write("  <quad%s%s%sp0=\"%d:3\" p1=\"%d:2\" p2=\"%.3f %.3f %.3f\" p3=\"%.3f %.3f %.3f\"/>\n" \
                    %(sInv,sAIIgnore if i == max_index - 1 else "",sDirection,self.global_quad_index_start+i-1, self.global_quad_index_start+i-1, \
                  r1[0],r1[2],r1[1], l1[0],l1[2],l1[1]) )
        if self.is_last_main:
            f.write("  <quad%sp0=\"%d:3\" p1=\"%d:2\" p2=\"0:1\" p3=\"0:0\"/>\n"\
                    % (sInv, self.global_quad_index_start+max_index-1, \
                             self.global_quad_index_start+max_index-1))

# ==============================================================================
# The actual exporter. It is using a class mainly to store some information
# between calls to different functions, e.g. a cache of exported objects.
class TrackExport:
    
    # Exports the models as spm object in local coordinate, i.e. with the object
    # center at (0,0,0).
    def exportLocalSPM(self, obj, sPath, name, applymodifiers=True):
        # If the name contains a ".spm" the model is assumed to be part of
        # the standard objects included in STK, so there is no need to
        # export the model.
        if re.search("\.spm$", name): return name
        
        name = name + ".spm"
        # If the object was already exported, we don't have to do it again.
        if name in self.dExportedObjects: return name
        
        if 'spm_export' not in dir(bpy.ops.screen):
            log_error("Cannot find the SPM exporter, make sure you installed it properly")
            return
        
        # FIXME: silly and ugly hack, the list of objects to export is passed through
        #        a custom scene property
        global the_scene
        the_scene.obj_list = [obj]
        
        try:
            bpy.ops.screen.spm_export(localsp=True, filepath=sPath+"/"+name,
                                      export_tangent=getSceneProperty(the_scene, 'precalculate_tangents', 'false') == 'true',
                                      overwrite_without_asking=True, applymodifiers=applymodifiers)
        except:
            log_error("Failed to export " + name)
            
        the_scene.obj_list = []
        #bpy.ops.screen.spm_export.skip_dialog = False
        #setObjList([])
        
        #spm_export.spm_parameters["local-space"] = old_space
        
        self.dExportedObjects[name]=1
        
        return name

    # ----------------------------------------------------------------------
    def writeTrackFile(self, sPath, nsBase):
        print("Writing track file --> \t")

        global the_scene

        #start_time  = bsys.time()
        scene       = the_scene
        name        = getSceneProperty(scene, "name",   "Name of Track")
        groups      = getSceneProperty(scene, "groups", "standard"     )
        if 'is_wip_track' in the_scene and the_scene['is_wip_track'] == 'true':
            groups = 'wip-track'
            
        is_arena    = getSceneProperty(scene, "arena",      "n"            )
        if not is_arena:
            is_arena="n"
        is_arena = not (is_arena[0]=="n" or is_arena[0]=="N" or \
                        is_arena[0]=="f" or is_arena[0]=="F"      )
                        
        is_soccer   = getSceneProperty(scene, "soccer",     "n"            )
        if not is_soccer:
            is_soccer="n"
        is_soccer = not (is_soccer[0]=="n" or is_soccer[0]=="N" or \
                         is_soccer[0]=="f" or is_soccer[0]=="F"      )

        is_ctf    = getSceneProperty(scene, "ctf",      "n"            )
        if not is_ctf:
            is_ctf="n"
        is_ctf = not (is_ctf[0]=="n" or is_ctf[0]=="N" or \
                      is_ctf[0]=="f" or is_ctf[0]=="F"      )

        is_cutscene = getSceneProperty(scene, "cutscene",  "false") == "true"
        is_internal = getSceneProperty(scene, "internal",   "n"            )
        is_internal = (is_internal == "true")
        if is_cutscene:
            is_internal = True
        
        push_back   = getSceneProperty(scene, "pushback",   "true"         )
        push_back   = (push_back != "false")
        
        auto_rescue = getSceneProperty(scene, "autorescue",   "true"       )
        auto_rescue = (auto_rescue != "false")
        
        designer    = getSceneProperty(scene, "designer",   ""             )
        
        # Support for multi-line descriptions:
        designer    = designer.replace("\\n", "\n")
        
        if not designer:
            designer    = getSceneProperty(scene, "description", "")
            if designer:
                log_warning("The 'Description' field is deprecated, please use 'Designer'")
            else:
                designer="?"
        
        music           = getSceneProperty(scene, "music", "")
        screenshot      = getSceneProperty(scene, "screenshot", "")
        smooth_normals  = getSceneProperty(scene, "smooth_normals", "false")
        #has_bloom       = (getSceneProperty(scene, "bloom", "false") == "true")
        bloom_threshold = getSceneProperty(scene, "bloom_threshold", "0.75")
        has_cloud_shadows = (getSceneProperty(scene, "clouds", "false") == "true")
        #has_lens_flare  = (getSceneProperty(scene, "sunlensflare", "false") == "true")
        has_shadows     = (getSceneProperty(scene, "shadows", "false") == "true")

        day_time        = getSceneProperty(scene, "duringday", "day")
        
        #has_colorlevel  = (getSceneProperty(scene, "colorlevel", "false") == "true")
        #colorlevel_inblack = getSceneProperty(scene, "colorlevel_inblack", "0.0")
        #colorlevel_ingamma = getSceneProperty(scene, "colorlevel_ingamma", "1.0")
        #colorlevel_inwhite = getSceneProperty(scene, "colorlevel_inwhite", "255.0")

        colorlevel_outblack = getSceneProperty(scene, "colorlevel_outblack", "0.0")
        colorlevel_outwhite = getSceneProperty(scene, "colorlevel_outwhite", "255.0")
        
        # Add default settings for sky-dome so that the user is aware of
        # can be set.
        getSceneProperty(scene, "sky_type", "dome")
        getSceneProperty(scene, "sky_texture", "" )
        getSceneProperty(scene, "sky_speed_x", "0")
        getSceneProperty(scene, "sky_speed_y", "0")
        # Not sure if these should be added - if the user wants a sky
        # box they are quiet annoying.
        #getSceneProperty(scene, "sky-color","")
        #getSceneProperty(scene, "sky-horizontal","")
        #getSceneProperty(scene, "sky-vertical", "")
        #getSceneProperty(scene, "sky-texture-percent","")
        #getSceneProperty(scene, "sky-sphere-percent", "")
        default_num_laps = int(getSceneProperty(scene, "default_num_laps",3))

        f = open(sPath+"/track.xml", mode='w', encoding='utf-8')
        f.write("<?xml version=\"1.0\"?>\n")
        f.write("<!-- Generated with script from SVN rev %s -->\n"%getScriptVersion())
        f.write("<track  name           = \"%s\"\n"%name)
        f.write("        version        = \"7\"\n")
        f.write("        groups         = \"%s\"\n"%groups)
        f.write("        designer       = \"%s\"\n"%designer)
        if music:
            f.write("        music          = \"%s\"\n"%music)
        else:
            log_warning("No music file defined.")

        if is_arena:
            f.write("        arena          = \"Y\"\n")
            
            max_arena_players = 0
            for obj in bpy.data.objects:
                stktype = getObjectProperty(obj, "type", "").strip().upper()
                if obj.type=="EMPTY" and stktype[:5]=="START":
                    if is_ctf and getObjectProperty(obj, "ctf_only", "false").lower() == "true":
                        continue
                    max_arena_players += 1
            
            f.write("        max-arena-players = \"%d\"\n" % max_arena_players)
        
        if is_soccer:
            f.write("        soccer         = \"Y\"\n")

        if is_ctf:
            f.write("        ctf            = \"Y\"\n")

        if is_cutscene:
            f.write("        cutscene       = \"Y\"\n")
        
        if is_internal:
            f.write("        internal       = \"Y\"\n")
        
        if not push_back:
            f.write("        push-back      = \"N\"\n")
        
        if not auto_rescue:
            f.write("        auto-rescue    = \"N\"\n")
            
        if screenshot:
            f.write("        screenshot     = \"%s\"\n"%screenshot)
        else:
            log_warning("No screenshot defined")

        f.write("        smooth-normals = \"%s\"\n" % smooth_normals)
        f.write("        default-number-of-laps = \"%d\"\n" % default_num_laps)
        
        reverse = getSceneProperty(scene, "reverse", "false")
        if reverse == "true":
            f.write("        reverse        = \"Y\"\n")
        else:
            f.write("        reverse        = \"N\"\n")
        
        #if has_bloom:
        #    f.write("        bloom          = \"Y\"\n")
        #    f.write("        bloom-threshold = \"%s\"\n" % bloom_threshold)
        #else:
        #    f.write("        bloom          = \"N\"\n")

        #if has_colorlevel:
        #    f.write("        color-level-in = \"" + str(colorlevel_inblack) + " " + str(colorlevel_ingamma) + " " + str(colorlevel_inwhite) + "\"\n")
        #    f.write("        color-level-out = \"" + str(colorlevel_outblack) + " " + str(colorlevel_outwhite) + "\"\n")

        if has_cloud_shadows:
            f.write("        clouds         = \"Y\"\n")
        else:
            f.write("        clouds         = \"N\"\n")
        
        #if has_lens_flare:
        #    f.write("        lens-flare     = \"Y\"\n")
        #else:
        #    f.write("        lens-flare     = \"N\"\n")
        
        if day_time == "day":
            f.write("        is-during-day  = \"Y\"\n")
        else:
            f.write("        is-during-day  = \"N\"\n")

        if has_shadows:
            f.write("        shadows        = \"Y\"\n")
        else:
            f.write("        shadows        = \"N\"\n")
        
        
        f.write(">\n")
        f.write("</track>\n")
        f.close()
        #print bsys.time() - start_time, "seconds"
     
    # --------------------------------------------------------------------------
    # Writes the animation for objects using IPOs:
    def writeAnimationWithIPO(self, f, name, obj, ipo, objectType="animation"):
        # An animated object can set the 'name' property, then this name will
        # be used to name the exported object (instead of the python name
        # which might be a default name with a number). Additionally, names
        # are cached so it can be avoided to export two or more identical
        # objects.
        parent = obj.parent
        
        flags = []
        
        # For now: armature animations are assumed to be looped
        if parent and parent.type=="ARMATURE":
            first_frame = the_scene.frame_start
            last_frame  = the_scene.frame_end
            frame_start = []
            frame_end = []
            for i in range(first_frame, last_frame + 1):
                for curr in the_scene.timeline_markers:
                    if curr.frame == i:
                        marker_name = curr.name.lower()
                        if marker_name == "start":
                            frame_start.append(i - 1)
                        if marker_name == "end":
                            frame_end.append(i - 1)
            if len(frame_start) > 0 and len(frame_end) > 0:
                flags.append('frame-start="%s"' % ' '.join(str(x) for x in frame_start))
                flags.append('frame-end="%s"' % ' '.join(str(x) for x in frame_end))
            is_cyclic = False
            if parent.animation_data is not None and parent.animation_data.action is not None and \
               parent.animation_data.action.fcurves is not None:
                for curve in parent.animation_data.action.fcurves:
                    for modifier in curve.modifiers:
                        if modifier.type == 'CYCLES':
                            is_cyclic = True
                            break
                    if is_cyclic:
                        break
            if is_cyclic:
                flags.append('looped="y"')
            
        interaction = getObjectProperty(obj, "interaction", 'static')
        flags.append('interaction="%s"' % interaction)
        # phyiscs only object can only have exact shape
        if interaction == "physicsonly":
            flags.append('shape="exact"')
        else:
            shape = getObjectProperty(obj, "shape", "")
            if shape and interaction != 'ghost':
                flags.append('shape="%s"'%shape)

        if not ipo: ipo=[]

        lodstring = self.getModelDefinitionString(obj)
        if len(lodstring) > 0:
            flags.append(lodstring)
        
        type = getObjectProperty(obj, "type", "")
        if type != "lod_instance":
            flags.append('model="%s"' % name)
        
        if interaction == 'reset':
            flags.append('reset="y"')
        elif interaction == 'explode':
            flags.append('explode="y"')
        elif interaction == 'flatten':
            flags.append('flatten="y"')
        
        if getObjectProperty(obj, "driveable", "false") == "true":
            flags.append('driveable="true"')

        if getObjectProperty(obj, "forcedbloom", "false") == "true":
            flags.append('forcedbloom="true"')
        
        if getObjectProperty(obj, "shadowpass", "true") == "false":
            flags.append('shadow-pass="false"')
        
        if len(getObjectProperty(obj, "outline", "")) > 0:
            flags.append('glow="%s"'%getObjectProperty(obj, "outline", ""))
            
        if getObjectProperty(obj, "displacing", "false") == "true":
            flags.append('displacing="true"')
            
        #if getObjectProperty(obj, "skyboxobject", "false") == "true":
        #    flags.append('renderpass="skybox"')
            
        if getObjectProperty(obj, "soccer_ball", "false") == "true":
            flags.append('soccer_ball="true"')
            
        uses_skeletal_animation = False
            
        # check if this object has an armature modifier
        for curr_mod in obj.modifiers:
            if curr_mod.type == 'ARMATURE':
                uses_skeletal_animation = True

        # check if this object has an armature parent (second way to do armature animations in blender)
        if obj.parent:
            if obj.parent.type == "ARMATURE":
                uses_skeletal_animation = True
        
        if uses_skeletal_animation:
            flags.append('skeletal-animation="true"')
        else:
            flags.append('skeletal-animation="false"')
            
        on_kart_collision = getObjectProperty(obj, "on_kart_collision", "")
        if len(on_kart_collision) > 0:
            flags.append("on-kart-collision=\"%s\""%on_kart_collision)
            
        custom_xml = getObjectProperty(obj, "custom_xml", "")
        if len(custom_xml) > 0:
            flags.append(custom_xml)
            
        if_condition = getObjectProperty(obj, "if", "")
        if len(if_condition) > 0:
            flags.append("if=\"%s\""%if_condition)

        lAnim = checkForAnimatedTextures([obj])
        detail_level = 0
        if getObjectProperty(obj, "enable_geo_detail", "false") == 'true':
            detail_level = int(getObjectProperty(obj, "geo_detail_level", 0))
        if detail_level > 0:
            flags.append("geometry-level=\"%d\"" % detail_level)

        if parent and parent.type=="ARMATURE":
            f.write("  <object id=\"%s\" type=\"%s\" %s %s>\n"% (obj.name, objectType, getXYZHPRString(parent), ' '.join(flags)))
        else:
            f.write("  <object id=\"%s\" type=\"%s\" %s %s>\n"% (obj.name, objectType, getXYZHPRString(obj), ' '.join(flags)))
            
        if lAnim:
            writeAnimatedTextures(f, lAnim)

        writeIPO(f, ipo)
        f.write("  </object>\n")
        
    # --------------------------------------------------------------------------
    
    def writeLODModels(self, f, sPath, lLODModels):
        for props in lLODModels:
            obj = props['object']
            spm_name = self.exportLocalSPM(obj, sPath, props['filename'], props['modifiers'])
            
            skeletal_anim_str = ""
            uses_skeletal_animation = False
            
            # check if this object has an armature modifier
            for curr_mod in obj.modifiers:
                if curr_mod.type == 'ARMATURE':
                    uses_skeletal_animation = True

            # check if this object has an armature parent (second way to do armature animations in blender)
            if obj.parent:
                if obj.parent.type == "ARMATURE":
                    uses_skeletal_animation = True
                    
            if uses_skeletal_animation:
                additional_prop_str = ' skeletal-animation="true"'
            else:
                additional_prop_str = ' skeletal-animation="false"'
            detail_level = 0
            if getObjectProperty(obj, "enable_geo_detail", "false") == 'true':
                detail_level = int(getObjectProperty(obj, "geo_detail_level", 0))
            if detail_level > 0:
                additional_prop_str += " geometry-level=\"%d\"" % detail_level
            
            f.write("    <static-object lod_distance=\"%i\" lod_group=\"%s\" model=\"%s\" %s interaction=\"%s\"%s/>\n" % (props['distance'], props['groupname'], spm_name, getXYZHPRString(obj), getObjectProperty(obj, "interaction", "static"), additional_prop_str) )

    # --------------------------------------------------------------------------
    # Write the objects that are part of the track (but not animated or
    # physical).
    def writeStaticObjects(self, f, sPath, lStaticObjects, lAnimTextures):
        for obj in lStaticObjects:
            
            lodstring = self.getModelDefinitionString(obj)

            # An object can set the 'name' property, then this name will
            # be used to name the exported object (instead of the python name
            # which might be a default name with a number). Additionally, names
            # are cached so it can be avoided to export two or more identical
            # objects.
            lAnim    = checkForAnimatedTextures([obj])
            name     = getObjectProperty(obj, "name", obj.name)
            if len(name) == 0: name = obj.name
            
            type = getObjectProperty(obj, "type", "X")
            
            if type != "lod_instance":
                spm_name = self.exportLocalSPM(obj, sPath, name, True)
            kind = getObjectProperty(obj, "kind", "")
            
            attributes = []
            attributes.append(lodstring)
            
            if type != "lod_instance" and type != "single_lod":
                attributes.append("model=\"%s\""%spm_name)

            attributes.append(getXYZHPRString(obj))
                
            condition_if = getObjectProperty(obj, "if", "")
            if len(condition_if) > 0:
                attributes.append("if=\"%s\""%condition_if)
            
            challenge_val = getObjectProperty(obj, "challenge", "")
            if len(challenge_val) > 0:
                attributes.append("challenge=\"%s\""% challenge_val)
            detail_level = 0
            if getObjectProperty(obj, "enable_geo_detail", "false") == 'true':
                detail_level = int(getObjectProperty(obj, "geo_detail_level", 0))
            if detail_level > 0:
                attributes.append("geometry-level=\"%d\"" % detail_level)
            interaction = getObjectProperty(obj, "interaction", '??')
            if interaction == 'reset':
                attributes.append("reset=\"y\"")
            elif interaction == 'explode':
                attributes.append("explode=\"y\"")
            elif interaction == 'flatten':
                attributes.append("flatten=\"y\"")
            if interaction == 'physicsonly':
                attributes.append('interaction="physics-only"')
            
            if lAnim:
                f.write("    <static-object %s>\n" % ' '.join(attributes))
                writeAnimatedTextures(f, lAnim)
                f.write("    </static-object>\n")
            else:
                f.write("    <static-object %s/>\n" % ' '.join(attributes))
        writeAnimatedTextures(f, lAnimTextures)

    # --------------------------------------------------------------------------
    # Get LOD string for a given object (returns an empty string if object is not LOD)
    def getModelDefinitionString(self, obj):
        lodstring = ""
        type = getObjectProperty(obj, "type", "object")
        if type == "lod_model":
            pass
        #elif type == "object" and getObjectProperty(obj, "instancing", "false") == "true":
        #    group = type = getObjectProperty(obj, "name", "")
        #    if len(group) == 0:
        #        log_warning("Instancing object " + obj.name + " has no name property")
        #    lodstring = ' instancing="true" instancing_model="' + group + '"'
        elif type == "lod_instance":
            group = type = getObjectProperty(obj, "lod_name", "")
            if len(group) == 0:
                log_warning("LOD instance " + obj.name + " has no group property")
            lodstring = ' lod_instance="true" lod_group="' + group + '"'
        elif type == "single_lod":
            lodstring = ' lod_instance="true" lod_group="_single_lod_' + getObjectProperty(obj, "name", obj.name) + '"'
        return lodstring

    

    
    # --------------------------------------------------------------------------
    # Writes a non-static track object. The objects can be animated or
    # non-animated meshes, and physical or non-physical.
    # Type is either 'movable' or 'nophysics'.
    def writeObject(self, f, sPath, obj):
        name     = getObjectProperty(obj, "name", obj.name)
        if len(name) == 0: name = obj.name
            
        type = getObjectProperty(obj, "type", "X")
        
        if obj.type != "CAMERA":
            if type == "lod_instance":
                spm_name = None
            else:
                spm_name = self.exportLocalSPM(obj, sPath, name, True)

        interact = getObjectProperty(obj, "interaction", "none")
        
        if obj.type=="CAMERA":
            ipo  = obj.animation_data
            self.writeAnimationWithIPO(f, "", obj, ipo, objectType="cutscene_camera")
        # An object that can be moved by the player. This object
        # can not have an IPO, so no need to test this here.
        elif interact=="move":
            ipo = obj.animation_data
            if ipo and ipo.action:
                log_warning("Movable object %s has an ipo - ipo is ignored." \
                            %obj.name)
            shape = getObjectProperty(obj, "shape", "")
            if not shape:
                log_warning("Movable object %s has no shape - box assumed!" \
                            % obj.name)
                shape="box"
            mass  = getObjectProperty(obj, "mass", 10)
            
            flags = []
            
            lodstring = self.getModelDefinitionString(obj)
            if len(lodstring) > 0:
                flags.append(lodstring)
            
            type = getObjectProperty(obj, "type", "?")
            
            if type != "lod_instance":
                flags.append('model="%s"' % spm_name)

            if getObjectProperty(obj, "forcedbloom", "false") == "true":
                flags.append('forcedbloom="true"')
            
            if getObjectProperty(obj, "shadowpass", "true") == "false":
                flags.append('shadow-pass="false"')
            
            if len(getObjectProperty(obj, "outline", "")) > 0:
                flags.append('glow="%s"'%getObjectProperty(obj, "outline", ""))
                
            if getObjectProperty(obj, "displacing", "false") == "true":
                flags.append('displacing="true"')
            
            #if getObjectProperty(obj, "skyboxobject", "false") == "true":
            #    flags.append('renderpass="skybox"')
                
            if getObjectProperty(obj, "soccer_ball", "false") == "true":
                flags.append('soccer_ball="true"')
                
            on_kart_collision = getObjectProperty(obj, "on_kart_collision", "")
            if len(on_kart_collision) > 0:
                flags.append("on-kart-collision=\"%s\""%on_kart_collision)
            
            custom_xml = getObjectProperty(obj, "custom_xml", "")
            if len(custom_xml) > 0:
                flags.append(custom_xml)
            
            if_condition = getObjectProperty(obj, "if", "")
            if len(if_condition) > 0:
                flags.append("if=\"%s\""%if_condition)
            
            uses_skeletal_animation = False
            
            # check if this object has an armature modifier
            for curr_mod in obj.modifiers:
                if curr_mod.type == 'ARMATURE':
                    uses_skeletal_animation = True

            # check if this object has an armature parent (second way to do armature animations in blender)
            if obj.parent:
                if obj.parent.type == "ARMATURE":
                    uses_skeletal_animation = True
            
            if uses_skeletal_animation:
                flags.append('skeletal-animation="true"')
            else:
                flags.append('skeletal-animation="false"')

            detail_level = 0
            if getObjectProperty(obj, "enable_geo_detail", "false") == 'true':
                detail_level = int(getObjectProperty(obj, "geo_detail_level", 0))
            if detail_level > 0:
                flags.append("geometry-level=\"%d\"" % detail_level)

            f.write('  <object type="movable" id=\"%s\" %s\n'% (obj.name, getXYZHPRString(obj)))
            f.write('          shape="%s" mass="%s" %s/>\n' % (shape, mass, ' '.join(flags)))
            
        # Now the object either has an IPO, or is a 'ghost' object.
        # Either can have an IPO. Even if the objects don't move
        # they are saved as animations (with 0 IPOs).
        elif interact=="ghost" or interact=="none" or interact=="static" or interact=="reset" or interact=="explode" or interact=="flatten" or interact=="physicsonly":
            
            ipo = obj.animation_data
            
            # In objects with skeletal animations the actual armature (which
            # is a parent) contains the IPO. So check for this:
            if not ipo or not ipo.action or not ipo.action.fcurves or len(ipo.action.fcurves) == 0:
                parent = obj.parent
                if parent:
                    ipo = parent.animation_data
            self.writeAnimationWithIPO(f, spm_name, obj, ipo)
            
        else:
            log_warning("Unknown interaction '%s' - ignored!"%interact)

          
    # --------------------------------------------------------------------------
    def writeEasterEggsFile(self, sPath, lEasterEggs):
        f = open(sPath+"/easter_eggs.xml", "w")
        f.write("<?xml version=\"1.0\"?>\n")
        f.write("<!-- Generated with script from SVN rev %s -->\n"%getScriptVersion())
        f.write("<EasterEggHunt>\n")
        
        #print("lEasterEggs : ", len(lEasterEggs), lEasterEggs);
        
        f.write("  <easy>\n")
        for obj in lEasterEggs:
            #print(getObjectProperty(obj, "easteregg_easy", "false"))
            if getObjectProperty(obj, "easteregg_easy", "false") == "true":
                f.write("    <easter-egg %s />\n" % getXYZHString(obj))
        f.write("  </easy>\n")
        
        f.write("  <medium>\n")
        for obj in lEasterEggs:
            #print(getObjectProperty(obj, "easteregg_medium", "false"))
            if getObjectProperty(obj, "easteregg_medium", "false") == "true":
                f.write("    <easter-egg %s />\n" % getXYZHString(obj))
        f.write("  </medium>\n")
        
        f.write("  <hard>\n")
        for obj in lEasterEggs:
            #print(getObjectProperty(obj, "easteregg_hard", "false"))
            if getObjectProperty(obj, "easteregg_hard", "false") == "true":
                f.write("    <easter-egg %s />\n" % getXYZHString(obj))
        f.write("  </hard>\n")
        
        f.write("</EasterEggHunt>\n")
        
        
    # --------------------------------------------------------------------------
    # Writes the scene files, which includes all models, animations, and items
    def writeSceneFile(self, sPath, sTrackName, exporters, lTrack, lObjects, lSun):

        #start_time = bsys.time()
        print("Writing scene file --> \t")
    
        is_lib_node = (getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') == 'true')
    
        filename = "scene.xml"
        if is_lib_node:
            filename = "node.xml"
        
        f = open(sPath + "/" + filename, "w")
        f.write("<?xml version=\"1.0\"?>\n")
        f.write("<!-- Generated with script from SVN rev %s -->\n"%getScriptVersion())
        f.write("<scene>\n")

        # Extract all static objects (which will be merged into one bullet object in stk):
        lStaticObjects = []
        # Include LOD models (i.e. the definition of a LOD group. Does not include LOD instances)
        lLODModels = {}
        #lInstancingModels = {}
        lOtherObjects  = []
        
        for obj in lObjects:
            type = getObjectProperty(obj, "type", "??")
            interact = getObjectProperty(obj, "interaction", "static")
            #if type == "lod_instance" or type == "lod_model" or type == "single_lod":
            #    interact = "static"
            
            # TODO: remove this fuzzy logic and let the artist clearly decide what is exported in the
            # track main model and what is exporter separately
            export_non_static = False
            if getObjectProperty(obj, "forcedbloom", "false") == "true":
                export_non_static = True
            elif getObjectProperty(obj, "shadowpass", "true") == "false":
                export_non_static = True
            elif len(getObjectProperty(obj, "outline", "")) > 0:
                export_non_static = True
            elif getObjectProperty(obj, "displacing", "false") == "true":
                export_non_static = True
            #elif getObjectProperty(obj, "skyboxobject", "false") == "true":
            #   export_non_static = True
            elif getObjectProperty(obj, "soccer_ball", "false") == "true":
               export_non_static = True
            elif is_lib_node:
                export_non_static = True
            elif interact=="reset" or interact=="explode" or interact=="flatten":
                export_non_static = True
            elif len(getObjectProperty(obj, "on_kart_collision", "")) > 0:
                export_non_static = True
            elif len(getObjectProperty(obj, "if", "")):
                export_non_static = True
            
            #if type == "object" and getObjectProperty(obj, "instancing", "false") == "true":
            #    if is_lib_node:
            #        instancing_name = getObjectProperty(obj, 'name', '')
            #        if len(instancing_name) == 0:
            #            log_warning('Object %s marked as instancing has no name' % obj.name)
            #            continue
            #        lInstancingModels[instancing_name] = obj
            #        lOtherObjects.append(obj)
            #    else:
            #        log_warning('Object %s marked as instancing. Instancing only works with library nodes.' % obj.name)
            #elif
            if type == 'lod_model':
                group_name = getObjectProperty(obj, 'lod_name', '')
                if len(group_name) == 0:
                    log_warning('Object %s marked as LOD but no LOD name specified' % obj.name)
                    continue
                if group_name not in lLODModels:
                    lLODModels[group_name] = []
                    
                lod_model_name = getObjectProperty(obj, "name", obj.name)
                loddistance = getObjectProperty(obj, "lod_distance", 60.0)
                if len(lod_model_name) == 0: lod_model_name = obj.name
                lLODModels[group_name].append({'object': obj, 'groupname': group_name, 'distance': loddistance, 'filename': lod_model_name, 'modifiers': True})
                
            elif type == 'single_lod':
                lod_model_name = getObjectProperty(obj, "name", obj.name)
                if len(lod_model_name) == 0: lod_model_name = obj.name
                
                group_name = "_single_lod_" + lod_model_name
                if group_name not in lLODModels:
                    lLODModels[group_name] = []
                    
                if getObjectProperty(obj, "nomodifierautolod", "false") == "true":
                    loddistance = getObjectProperty(obj, "nomodierlod_distance", 30.0)
                    lLODModels[group_name].append({'object': obj, 'groupname': group_name, 'distance': loddistance, 'filename': lod_model_name, 'modifiers': True})
                    loddistance = getObjectProperty(obj, "lod_distance", 60.0)
                    lLODModels[group_name].append({'object': obj, 'groupname': group_name, 'distance': loddistance, 'filename': lod_model_name + "_mid", 'modifiers': False})
                else:
                    loddistance = getObjectProperty(obj, "lod_distance", 60.0)
                    lLODModels[group_name].append({'object': obj, 'groupname': group_name, 'distance': loddistance, 'filename': lod_model_name, 'modifiers': True})
                
                
                # this object is both a model and an instance, so also add it to the list of objects, where it will be exported as a LOD instance
                if export_non_static:
                    lOtherObjects.append(obj)
                else:
                    lStaticObjects.append(obj)
                
            elif not export_non_static and (interact=="static" or type == "lod_model" or interact=="physicsonly"):
                
                ipo = obj.animation_data
                if obj.parent is not None and obj.parent.type=="ARMATURE" and obj.parent.animation_data is not None:
                    ipo = obj.parent.animation_data
                
                # If an static object has an IPO, it will be moved, and
                # can't be merged with the physics model of the track
                if (ipo and ipo.action):
                    lOtherObjects.append(obj)
                else:
                    lStaticObjects.append(obj)
            else:
                lOtherObjects.append(obj)
                
        lAnimTextures  = checkForAnimatedTextures(lTrack)
        
        if len(lLODModels.keys()) > 0:
            f.write('  <lod>\n')
            for group_name in lLODModels.keys():
                lLODModels[group_name].sort(key = lambda a: a['distance'])
                f.write('   <group name="%s">\n' % group_name)
                self.writeLODModels(f, sPath, lLODModels[group_name])
                f.write('   </group>\n')
            f.write('  </lod>\n')
        
        #if len(lInstancingModels.keys()) > 0:
        #    f.write('  <instancing>\n')
        #    for instancing_name in lInstancingModels.keys():
        #        f.write('   <group name="%s">\n' % instancing_name)
        #        self.writeInstancingModel(f, sPath, instancing_name, lInstancingModels[instancing_name])
        #        f.write('   </group>\n')
        #    f.write('  </instancing>\n')
        
        if getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true':
            if lStaticObjects or lAnimTextures:
                f.write("  <track model=\"%s\" x=\"0\" y=\"0\" z=\"0\">\n"%sTrackName)
                self.writeStaticObjects(f, sPath, lStaticObjects, lAnimTextures)
                f.write("  </track>\n")
            else:
                f.write("  <track model=\"%s\" x=\"0\" y=\"0\" z=\"0\"/>\n"%sTrackName)
        
        for obj in lOtherObjects:
            self.writeObject(f, sPath, obj)

        # Subtitles
        subtitles = []
        end_time = bpy.data.scenes[0].frame_end
        for marker in reversed(bpy.data.scenes[0].timeline_markers):
            if marker.name.startswith("subtitle"):
                subtitle_text = bpy.data.scenes[0][marker.name]
                subtitles.insert(0, [marker.frame, end_time - 1, subtitle_text])
            end_time = marker.frame
        
        if len(subtitles) > 0:
            f.write("  <subtitles>\n")
            
            for subtitle in subtitles:
                f.write("        <subtitle from=\"%i\" to=\"%i\" text=\"%s\"/>\n" % (subtitle[0], subtitle[1], subtitle[2]))
            
            f.write("  </subtitles>\n")
        
        
        # Assemble all sky/fog related parameters
        # ---------------------------------------
        if len(lSun) > 1:
            log_warning("Warning: more than one Sun defined, only the first will be used."   )         
        sSky=""
        global the_scene
        scene = the_scene
        s = getSceneProperty(scene, "fog", 0)
        if s == "yes" or s == "true":
            sSky="%s fog=\"true\""%sSky
            s=getSceneProperty(scene, "fog_color", 0)
            if s: sSky="%s fog-color=\"%s\""%(sSky, s)
            s=float(getSceneProperty(scene, "fog_max", 0))
            if s: sSky="%s fog-max=\"%s\""%(sSky, s)
            s=float(getSceneProperty(scene, "fog_start", 0))
            if s: sSky="%s fog-start=\"%.2f\""%(sSky, s)
            s=float(getSceneProperty(scene, "fog_end", 0))
            if s: sSky="%s fog-end=\"%.2f\""%(sSky, s)

        # If there is a sun:
        if len(lSun) > 0:
            sun = lSun[0]
            xyz=sun.location
            sSky="%s xyz=\"%.2f %.2f %.2f\""%(sSky, float(xyz[0]), float(xyz[2]), float(xyz[1]))
            s=getObjectProperty(sun, "color", 0)
            if s: sSky="%s sun-color=\"%s\""%(sSky, s)
            s=getObjectProperty(sun, "specular", 0)
            if s: sSky="%s sun-specular=\"%s\""%(sSky, s)
            s=getObjectProperty(sun, "diffuse", 0)
            if s: sSky="%s sun-diffuse=\"%s\""%(sSky, s)
            s=getObjectProperty(sun, "ambient", 0)
            if s: sSky="%s ambient=\"%s\""%(sSky, s)

        if sSky:
            f.write("  <sun %s/>\n"%sSky)
            
        sky_color=getSceneProperty(scene, "sky_color", None)
        if sky_color:
            f.write("  <sky-color rgb=\"%s\"/>\n"%sky_color)

        weather = ""
        weather_type = getSceneProperty(scene, "weather_type", "none")
        if weather_type != "none":
            if weather_type[:4] != ".xml":
                weather_type = weather_type + ".xml"
            weather = " particles=\"" + weather_type + "\""
                
        lightning = getSceneProperty(scene, "weather_lightning", "false")
        if lightning == "true":
            weather = weather + " lightning=\"true\""
        
        weather_sound = getSceneProperty(scene, "weather_sound", "")
        if weather_sound != "":
            weather = weather + " sound=\"" + weather_sound + "\""
        
        if weather != "":
            f.write("  <weather%s/>\n"%weather)
        
        rad2deg = 180.0/3.1415926

        
        scene   = the_scene
        sky     = getSceneProperty(scene, "sky_type", None)
        
        sphericalHarmonicsStr = ""
        if getSceneProperty(scene, "ambientmap", "false") == "true":
            sphericalHarmonicsTextures = []
            s = getSceneProperty(scene, "ambientmap_texture2", "")
            if len(s) > 0: sphericalHarmonicsTextures.append(s)
            s = getSceneProperty(scene, "ambientmap_texture3", "")
            if len(s) > 0: sphericalHarmonicsTextures.append(s)
            s = getSceneProperty(scene, "ambientmap_texture4", "")
            if len(s) > 0: sphericalHarmonicsTextures.append(s)
            s = getSceneProperty(scene, "ambientmap_texture5", "")
            if len(s) > 0: sphericalHarmonicsTextures.append(s)
            s = getSceneProperty(scene, "ambientmap_texture6", "")
            if len(s) > 0: sphericalHarmonicsTextures.append(s)
            s = getSceneProperty(scene, "ambientmap_texture1", "")
            if len(s) > 0: sphericalHarmonicsTextures.append(s)
            if len(sphericalHarmonicsTextures) == 6:
                sphericalHarmonicsStr = 'sh-texture="' + " ".join(sphericalHarmonicsTextures) + '"'
            else:
                log_warning('Invalid ambient map textures')
        
        # Note that there is a limit to the length of id properties,
        # which can easily be exceeded by 6 sky textures for a full sky box.
        # Therefore also check for sky-texture1 and sky-texture2.
        texture = getSceneProperty(scene, "sky_texture", "")
        s       = getSceneProperty(scene, "sky_texture1", "")
        if s: texture = "%s %s"%(texture, s)
        s       = getSceneProperty(scene, "sky_texture2", "")
        if s: texture = "%s %s"%(texture, s)
        if sky and texture:
            if sky=="dome":
                hori           = getSceneProperty(scene, "sky_horizontal",     16  )
                verti          = getSceneProperty(scene, "sky_vertical",       16  )
                tex_percent    = getSceneProperty(scene, "sky_texture_percent", 0.5)
                sphere_percent = getSceneProperty(scene, "sky_sphere_percent",  1.3)
                speed_x        = getSceneProperty(scene, "sky_speed_x",         0.0)
                speed_y        = getSceneProperty(scene, "sky_speed_y",         0.0)
                f.write("""
  <sky-dome texture=\"%s\" %s
            horizontal=\"%s\" vertical=\"%s\" 
            texture-percent=\"%s\" sphere-percent=\"%s\"
            speed-x=\"%s\" speed-y=\"%s\" />
""" %(texture, sphericalHarmonicsStr, hori, verti, tex_percent, sphere_percent, speed_x, speed_y))
            elif sky=="box":
                lTextures = [getSceneProperty(scene, "sky_texture2", ""),
                             getSceneProperty(scene, "sky_texture3", ""),
                             getSceneProperty(scene, "sky_texture4", ""),
                             getSceneProperty(scene, "sky_texture5", ""),
                             getSceneProperty(scene, "sky_texture6", ""),
                             getSceneProperty(scene, "sky_texture1", "")]
                f.write("  <sky-box texture=\"%s\" %s/>\n" % (" ".join(lTextures), sphericalHarmonicsStr))
                
        camera_far  = getSceneProperty(scene, "camera_far", ""             )
        if camera_far:            
            f.write("  <camera far=\"%s\"/>\n"%camera_far)
        
        for exporter in exporters:
            exporter.export(f)
        
        f.write("</scene>\n")
        f.close()
        #print bsys.time()-start_time,"seconds"

    
    def __init__(self, sFilename, exportImages, exportDrivelines, exportScene, exportMaterials):
        self.dExportedObjects = {}
        
        sBase = os.path.basename(sFilename)
        sPath = os.path.dirname(sFilename)
        
        stk_delete_old_files_on_export = False
        try:
            stk_delete_old_files_on_export = bpy.context.user_preferences.addons['stk_track'].preferences.stk_delete_old_files_on_export
        except:
            pass
            
        if stk_delete_old_files_on_export:
            os.chdir(sPath)
            old_model_files = [ f for f in os.listdir(sPath) if f.endswith(".spm") ]
            for f in old_model_files:
                print("Deleting ", f)
                os.remove(f)
        
        blendfile_dir = os.path.dirname(bpy.data.filepath)
        
        import shutil
        if exportImages:
            for i,curr in enumerate(bpy.data.images):
                try:
                    if curr.filepath is None or len(curr.filepath) == 0:
                        continue
                    
                    abs_texture_path = bpy.path.abspath(curr.filepath)
                    print('abs_texture_path', abs_texture_path, blendfile_dir)
                    if bpy.path.is_subdir(abs_texture_path, blendfile_dir):
                        shutil.copy(abs_texture_path, sPath)
                except:
                    import traceback
                    traceback.print_exc(file=sys.stdout)
                    log_warning('Failed to copy texture ' + curr.filepath)
        
        drivelineExporter = DrivelineExporter()
        navmeshExporter = NavmeshExporter()
        exporters = [drivelineExporter, WaterExporter(self, sPath), ParticleEmitterExporter(), BlenderHairExporter(), SoundEmitterExporter(),
                     ActionTriggerExporter(), ItemsExporter(), BillboardExporter(), LightsExporter(), LightShaftExporter(),
                     StartPositionFlagExporter(), LibraryNodeExporter(), navmeshExporter]
        
        # Collect the different kind of meshes this exporter handles
        # ----------------------------------------------------------
        lObj                 = bpy.data.objects      # List of all objects
        lTrack               = []                    # All main track objects
        lCameraCurves        = []                    # Camera curves (unused atm)
        lObjects             = []                    # All special objects
        lSun                 = []
        lEasterEggs          = []
        
        for obj in lObj:
            # Try to get the supertuxkart type field. If it's not defined,
            # use the name of the objects as type.
            stktype = getObjectProperty(obj, "type", "").strip().upper()
            
            #print("Checking object",obj.name,"which has type",stktype)

            # Make it possible to ignore certain objects, e.g. if you keep a
            # selection of 'templates' (ready to go models) around to be
            # copied into the main track.
            if stktype=="IGNORE": continue
            
            # Do not export linked objects; linked objects will be used as
            # templates to create instances from
            if obj.library is not None:
                continue
            
            if stktype=="EASTEREGG":
                lEasterEggs.append(obj)
                continue
                
            objectProcessed = False
            for exporter in exporters:
                if exporter.processObject(obj, stktype):
                    objectProcessed = True
                    break
            if objectProcessed:
                continue
            
            if obj.type=="LAMP" and stktype == "SUN":
                lSun.append(obj)
                continue
            elif obj.type=="CAMERA" and stktype == 'CUTSCENE_CAMERA':
                lObjects.append(obj)
                continue
            elif obj.type!="MESH":
                #print "Non-mesh object '%s' (type: '%s') is ignored!"%(obj.name, stktype)
                continue
            
            if stktype=="OBJECT" or stktype=="SPECIAL_OBJECT" or stktype=="LOD_MODEL" or stktype=="LOD_INSTANCE" or stktype=="SINGLE_LOD":
                lObjects.append(obj)
            elif stktype=="CANNONEND":
                pass # cannon ends are handled with cannon start objects
            elif stktype=="NONE":
                lTrack.append(obj)
            else:
                s = getObjectProperty(obj, "type", None)
                if s:
                    log_warning("object " + obj.name + " has type property '%s', which is not supported.\n"%s)
                lTrack.append(obj)

        is_arena = getSceneProperty(bpy.data.scenes[0], "arena", "false") == "true"
        is_soccer = getSceneProperty(bpy.data.scenes[0], "soccer", "false") == "true"
        is_cutscene = getSceneProperty(bpy.data.scenes[0], "cutscene",  "false") == "true"

        # Now export the different parts: track file
        # ------------------------------------------
        if exportScene and getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true':
            self.writeTrackFile(sPath, sBase)
    
        # Quads and mapping files
        # -----------------------
        global the_scene
        scene    = the_scene
        
        is_arena = getSceneProperty(scene, "arena", "n")
        if not is_arena: is_arena="n"
        is_arena = not (is_arena[0]=="n" or is_arena[0]=="N" or \
                        is_arena[0]=="f" or is_arena[0]=="F"     )
                        
        is_soccer = getSceneProperty(scene, "soccer", "n")
        if not is_soccer: is_soccer="n"
        is_soccer = not (is_soccer[0]=="n" or is_soccer[0]=="N" or \
                         is_soccer[0]=="f" or is_soccer[0]=="F"     )

        if exportDrivelines and not is_arena and not is_soccer and not is_cutscene:
            drivelineExporter.writeQuadAndGraph(sPath)
        if (is_arena or is_soccer):
            navmeshExporter.exportNavmesh(sPath)
            
        #start_time = bsys.time()

        sTrackName = sBase+"_track.spm"

        # FIXME: silly and ugly hack, the list of objects to export is passed through
        #        a custom scene property
        scene.obj_list = lTrack
        
        if 'spm_export' not in dir(bpy.ops.screen):
            log_error("Cannot find the SPM exporter, make sure you installed it properly")
            return
        
        if exportScene and getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true':
            bpy.ops.screen.spm_export(localsp=False, filepath=sPath+"/"+sTrackName, do_sp=False,
                                      export_tangent=getSceneProperty(scene, 'precalculate_tangents', 'false') == 'true',
                                      overwrite_without_asking=True)
        scene.obj_list = []
        
        #write_spm_file(sFilename+"_track.spm")
        
        #spm_export.write_spm_file(sFilename+"_track.spm", lTrack)
        #print bsys.time()-start_time,"seconds."
    
        # scene file
        # ----------
        if exportScene:
            self.writeSceneFile(sPath, sTrackName, exporters, lTrack, lObjects, lSun)
            
            if len(lEasterEggs) > 0 and getSceneProperty(scene, 'is_stk_node', 'false') != 'true':
                self.writeEasterEggsFile(sPath, lEasterEggs)
        
        # materials file
        # ----------
        if 'stk_material_exporter' not in dir(bpy.ops.screen):
            log_error("Cannot find the material exporter, make sure you installed it properly")
            return
        
        if exportMaterials:
            bpy.ops.screen.stk_material_exporter(filepath=sPath)

        import datetime
        now = datetime.datetime.now()
        log_info("Export completed on " + now.strftime("%Y-%m-%d %H:%M"))
        print("Finished.")


        
# ==============================================================================
def savescene_callback(sFilename, exportImages, exportDrivelines, exportScene, exportMaterials):
    global log
    log = []
    
    TrackExport(sFilename, exportImages, exportDrivelines and getSceneProperty(bpy.data.scenes[0], 'is_stk_node', 'false') != 'true', exportScene, exportMaterials)

thelist = []
def getlist(self):
    global thelist
    return thelist
def setlist(self, value):
    global thelist
    thelist = value
    
# ==== EXPORT OPERATOR ====
class STK_Track_Export_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_track_export")
    bl_label = ("SuperTuxKart Track Export")
    filepath = bpy.props.StringProperty(subtype="FILE_PATH")
    exportScene = bpy.props.BoolProperty(name="Export scene", default=True)
    exportDrivelines = bpy.props.BoolProperty(name="Export drivelines", default=True)
    exportMaterials = bpy.props.BoolProperty(name="Export materials", default=True)
    
    def invoke(self, context, event):
        if bpy.context.mode != 'OBJECT':
            self.report({'ERROR'}, "You must be in object mode")
            log_error("You must be in object mode")
            return {'FINISHED'}
        
        isATrack = ('is_stk_track' in context.scene) and (context.scene['is_stk_track'] == 'true')
        isANode = ('is_stk_node' in context.scene) and (context.scene['is_stk_node'] == 'true')
        
        if not isATrack and not isANode:
            log_error("Not a STK library node or a track!")
            return {'FINISHED'}
        
        # FIXME: in library nodes it's "name", in tracks it's "code"
        if isANode:
            if 'name' not in context.scene or len(context.scene['name']) == 0:
                self.report({'ERROR'}, "Please specify a name")
                log_error("Please specify a name")
                return {'FINISHED'}
            code = context.scene['name']
        else:
            if 'code' not in context.scene or len(context.scene['code']) == 0:
                self.report({'ERROR'}, "Please specify a code name (folder name)")
                log_error("Please specify a code name (folder name)")
                return {'FINISHED'}
            code = context.scene['code']
        
        assets_path = ""
        try:
            assets_path = bpy.context.user_preferences.addons['stk_track'].preferences.stk_assets_path
        except:
            pass
            
        if assets_path is None or len(assets_path) == 0:
            self.report({'ERROR'}, "Please select the export path in the export panel")
            log_error("Please select the export path in the export panel")
            return {'FINISHED'}
        
        if isANode:
            folder = os.path.join(assets_path, 'library', code)
        else:
            if 'is_wip_track' in context.scene and context.scene['is_wip_track'] == 'true':
                folder = os.path.join(assets_path, 'wip-tracks', code)
            else:
                folder = os.path.join(assets_path, 'tracks', code)
            
        if not os.path.exists(folder):
            os.makedirs(folder)
        self.filepath = os.path.join(folder, code)
        return self.execute(context)
        
        #if isANode:
        #    # library node
        #    folder = os.path.join(bpy.context.user_preferences.addons['stk_track'].preferences.stk_assets_path, 'library', context.scene['code'])
        #    
        #    if not os.path.exists(folder):
        #        os.makedirs(folder)
        #    
        #    self.filepath = os.path.join(folder, context.scene['code'])
        #    
        #    return self.execute(context)
        #else:
        #    # track
        #    self.filepath = os.path.join(bpy.context.user_preferences.addons['stk_track'].preferences.stk_assets_path, 'tracks')
        #    
        #    context.window_manager.fileselect_add(self)
        #    return {'RUNNING_MODAL'}

    def execute(self, context):
        if bpy.context.mode != 'OBJECT':
            self.report({'ERROR'}, "You must be in object mode")
            return {'FINISHED'}
        
        isNotATrack = ('is_stk_track' not in context.scene) or (context.scene['is_stk_track'] != 'true')
        isNotANode = ('is_stk_node' not in context.scene) or (context.scene['is_stk_node'] != 'true')

        if self.filepath == "" or (isNotATrack and isNotANode):
            return {'FINISHED'}
        
        global operator
        operator = self
        
        # FIXME: silly and ugly hack, the list of objects to export is passed through
        #        a custom scene property
        bpy.types.Scene.obj_list = property(getlist, setlist)
        
        exportImages = bpy.data.scenes[0].stk_track_export_images
        savescene_callback(self.filepath, exportImages, self.exportDrivelines, self.exportScene, self.exportMaterials)
        return {'FINISHED'}


class STK_Copy_Log_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_track_copy_log")
    bl_label = ("Copy Log")

    def execute(self, context):
        global log
        context.window_manager.clipboard = str(log)
        return {'FINISHED'}

class STK_Clean_Log_Operator(bpy.types.Operator):
    bl_idname = ("screen.stk_track_clean_log")
    bl_label = ("Clean Log")

    def execute(self, context):
        global log
        log = []
        print("Log cleaned")
        return {'FINISHED'}

class STK_FolderPicker_Operator(bpy.types.Operator):
    bl_idname = "screen.stk_pick_assets_path"
    bl_label = "Select the SuperTuxKart assets (data) folder"

    filepath = bpy.props.StringProperty(subtype="DIR_PATH")

    @classmethod
    def poll(cls, context):
        return True

    def execute(self, context):
        import bpy.path
        import os.path
        user_preferences = context.user_preferences
        addon_prefs = user_preferences.addons['stk_track'].preferences
        addon_prefs.stk_assets_path = os.path.dirname(bpy.path.abspath(self.filepath))
        bpy.ops.wm.save_userpref()
        return {'FINISHED'}

    def invoke(self, context, event):
        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}
        
        
# ==== TRACK EXPORT PANEL ====
class STK_Track_Exporter_Panel(bpy.types.Panel):
    bl_label = "Track Exporter"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    
    def draw(self, context):
        global the_scene
        the_scene = context.scene
        
        isNotANode = ('is_stk_node' not in context.scene) or (context.scene['is_stk_node'] != 'true')
        if isNotANode:
            self.bl_label = "Track Exporter"
        else:
            self.bl_label = "Library Node Exporter"
        
        layout = self.layout
        
        # ==== Types group ====
        row = layout.row()
        
        assets_path = ""
        try:
            assets_path = bpy.context.user_preferences.addons['stk_track'].preferences.stk_assets_path
        except:
            pass
            
        if assets_path is not None and len(assets_path) > 0:
            row.label('Assets path: ' + assets_path)
        else:
            row.label('Assets path: [please select path]') 
        row.operator('screen.stk_pick_assets_path', icon='FILESEL', text='')
        
        if assets_path is None or len(assets_path) == 0:
            return
        
        row = layout.row()
        row.prop(the_scene, 'stk_track_export_images', text="Copy texture files")
        
        row = layout.row()
        
        if isNotANode:
            row.operator("screen.stk_track_export", "Export track", icon='BLENDER')
        else:
            row.operator("screen.stk_track_export", "Export library node", icon='BLENDER')
        
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
            row.operator("screen.stk_track_clean_log", text="Clear Log", icon='X')
            row.operator("screen.stk_track_copy_log",  text="Copy Log", icon='COPYDOWN')


# Add to a menu
def menu_func_export_stktrack(self, context):
    global the_scene
    the_scene = context.scene
    self.layout.operator(STK_Track_Export_Operator.bl_idname, text="STK Track")

def register():
    bpy.types.Scene.stk_track_export_images = bpy.props.BoolProperty(name="Export images")
    bpy.types.INFO_MT_file_export.append(menu_func_export_stktrack)
    bpy.utils.register_module(__name__)

def unregister():
    bpy.types.INFO_MT_file_export.remove(menu_func_export_stktrack)

if __name__ == "__main__":
    register()
