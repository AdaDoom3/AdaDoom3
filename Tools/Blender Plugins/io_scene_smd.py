bl_info = {
    "name": "SMD: Valve studiomodel source format",
    "author": "nemyax",
    "version": (0, 1, 20161113),
    "blender": (2, 7, 7),
    "location": "File > Import-Export",
    "description": "Export Valve studiomodel sources",
    "warning": "",
    "wiki_url": "https://sourceforge.net/p/blenderbitsbobs/wiki/SMD%20exporter",
    "tracker_url": "",
    "category": "Import-Export"}

import bpy
import os
import bmesh
import mathutils as mu
import math
import re
from bpy.props import (
    BoolProperty,
    FloatProperty,
    StringProperty,
    IntProperty)
from bpy_extras.io_utils import (
    ExportHelper,
    path_reference_mode)

###
### Export functions
###

def get_ranges(marker_filter):
    markers = bpy.context.scene.timeline_markers
    starts = [m for m in markers if
        m.name.startswith(marker_filter) and
        m.name.endswith("_start", 1)]
    ends = [m for m in markers if
        m.name.startswith(marker_filter) and
        m.name.endswith("_end", 1)]
    if not starts or not ends:
        return {}
    return find_matches(starts, ends)

def find_matches(starts, ends):
    pairs = {}
    for s in starts:
        basename = s.name[:s.name.rfind("_start")]
        matches = [e for e in ends if
            e.name[:e.name.rfind("_end")] == basename]
        if matches:
            m = matches[0]
            pairs[basename] = (min(s.frame, m.frame),max(s.frame, m.frame))
    return pairs

def get_node_items(bone_lu):
    items = []
    for name in bone_lu:
        idx, par = bone_lu[name][:2]
        items.append((idx,name,par))
    items.sort()
    result = []
    template = "{} \"{}\" {}\n"
    for a, b, c in items:
        result.append(template.format(a, b, c))
    return result

def xf_from_rest_pose(bone):
    if not bone:
        return [0.0] * 6
    par = bone.parent
    if par:
        mtx = par.bone.matrix_local.inverted() * bone.bone.matrix_local
    else:
        mtx = bone.bone.matrix_local
    return mtx.to_translation()[:] + mtx.to_euler()[:]

def xf_from_live_pose(bone):
    par = bone.parent
    if par:
        mtx = par.matrix.inverted() * bone.matrix
    else:
        mtx = bone.matrix
    return mtx.to_translation()[:] + mtx.to_euler()[:]

def get_xform_items(bones, frame=None):
    template = "{}" + " {:.6f}" * 6 + "\n"
    if frame == None:
        frame = 0
        xf_query_fn = xf_from_rest_pose
    else:
        xf_query_fn = xf_from_live_pose
    result = ["time {}\n".format(frame)]
    for idx, bone in bones:
        tx, ty, tz, rx, ry, rz = xf_query_fn(bone)
        result.append(template.format(idx, tx, ty, tz, rx, ry, rz))
    return result

def get_tri_items(bm, tx_lu, weighting=False):
    uv = bm.loops.layers.uv.verify()
    dl = bm.verts.layers.deform.active
    template = "{}" + " {:.6f}" * 8
    result = []
    for f in bm.faces:
        mi = f.material_index
        if mi in tx_lu:
            result.append(tx_lu[mi] + "\n")
        else:
            result.append("unknown\n")
        for l in f.loops:
            vert = l.vert
            wts = vert[dl].items()
            pb = sorted([(b,a) for a, b in wts])[-1][1]
            vx, vy, vz = vert.co
            nx, ny, nz = l.vert.normal
            u, v = l[uv].uv
            entry = template.format(pb, vx, vy, vz, nx, ny, nz, u, v)
            if weighting:
                entry += " {}".format(len(wts))
                for k, v in wts:
                    entry += " {} {:.6f}".format(k, v)
            entry += "\n"
            result.append(entry)
    return result

def write_smd_mesh(path, prerequisites, weighting):
    bone_lu, tx_lu, bm = prerequisites
    lines = ["version 1\n"]
    lines.append("nodes\n")
    lines.extend(get_node_items(bone_lu))
    lines.append("end\n")
    lines.append("skeleton\n")
    bones = [(a[0],a[2]) for a in bone_lu.values()]
    bones.sort()
    lines.extend(get_xform_items(bones))
    lines.append("end\n")
    lines.append("triangles\n")
    lines.extend(get_tri_items(bm, tx_lu, weighting))
    lines.append("end\n")
    f = open(path, 'w')
    for line in lines:
        f.write(line)
    f.close()

def write_smd_anim(path, prerequisites, frame_range):
    bone_lu, tx_lu, bm = prerequisites
    go_back = bpy.context.scene.frame_current
    if frame_range == None:
        start_frame = bpy.context.scene.frame_start
        end_frame = bpy.context.scene.frame_end
    else:
        start_frame, end_frame = frame_range
    lines = ["version 1\n"]
    lines.append("nodes\n")
    lines.extend(get_node_items(bone_lu))
    lines.append("end\n")
    lines.append("skeleton\n")
    bones = [(a[0],a[2]) for a in bone_lu.values()]
    bones.sort()
    if bones == [(0,None)]:
        lines.append("time 0\n")
        lines.append("0"+" 0.000000"*6+"\n")
    else:
        for frame in range(start_frame, end_frame + 1):
            bpy.context.scene.frame_set(frame)
            lines.extend(get_xform_items(bones, frame - start_frame))
    lines.append("end\n")
    f = open(path, 'w')
    for line in lines:
        f.write(line)
    bpy.context.scene.frame_set(go_back)
    f.close()

def write_qc(path, ranges):
    path_noext = os.path.splitext(path)[0]
    bn = os.path.basename(path_noext)
    qc_str = "".join([
        "$modelname \"{}\"\n".format(bn), 
        "$cd \".\\\"\n",
        "$cdtexture \".\\\"\n",
        "$scale 1.0\n",
        "$origin 0 0 0 0\n",
        "$bodygroup body\n",
        "{\n"+"studio \"{}\"\n".format(bn)+"}\n"])
    fps = bpy.context.scene.render.fps
    template = "$sequence "+"\"{0}\" "*2+"fps {}\n".format(fps)
    for a in ranges:
        qc_str += template.format(a)
    qc_path = "{}.qc".format(path_noext)
    f = open(qc_path, 'w')
    f.write(qc_str)
    f.close()

def write_batch(path, prerequisites, marker_filter, weighting, qc):
    bone_lu, tx_lu, bm = prerequisites
    write_smd_mesh(path, prerequisites, weighting)
    ranges = get_ranges(marker_filter)
    if not ranges:
        ranges = {'idle':(1,1)}
    for r in ranges:
        folder = os.path.dirname(path)
        anim_file = os.path.join(folder, r + ".smd")
        write_smd_anim(anim_file, prerequisites, ranges[r])
    if qc:
        write_qc(path, ranges)
    return {'FINISHED'}

def is_export_go():
    obj = bpy.context.active_object
    if not obj or obj.type != 'MESH':
        return 'no_obj', None
    arm = obj.find_armature()
    bones = get_bones(arm)
    root_bones = [i for i in bones if i.parent not in bones]
    if len(root_bones) > 1:
        root_items = [" - {}\n".format(rb.name) for rb in root_bones]
        return 'roots', root_items
    return 'ok', None

def get_prereqs(what):
    obj = bpy.context.active_object
    arm = obj.find_armature()
    bones = get_bones(arm)
    if bones:
        bone_lu = make_bone_lookup(bones)
    else:
        bone_lu = {"--base":(0,-1,None)}
    tx_lu = bm = None # not needed if anim only
    if what != 'anim':
        if arm:
            states = dict([(m,m.show_viewport) for m in obj.modifiers
                if m.type == 'ARMATURE'])
            for m in states:
                m.show_viewport = False
            tx_lu, bm = prep_bmesh(obj, bone_lu)
            for m in states:
                m.show_viewport = states[m]
        else:
            tx_lu, bm = prep_bmesh(obj, bone_lu)
    return bone_lu, tx_lu, bm

###
### Helper functions
###

def strip_wires(bm):
    [bm.faces.remove(f) for f in bm.faces if len(f.verts) < 3]
    [bm.edges.remove(e) for e in bm.edges if not e.link_faces[:]]
    [bm.verts.remove(v) for v in bm.verts if v.is_wire]
    for seq in [bm.verts, bm.faces, bm.edges]: seq.index_update()
    return bm

def triangulate(bm):
    nontris = [f for f in bm.faces if len(f.verts) > 3]
    bmesh.ops.triangulate(bm, faces=nontris)
    return bm

def make_texture_lookup(mat_slots):
    result = {}
    mi = 0
    for ms in mat_slots:
        mat = ms.material
        if mat:
            result[mi] = mat.name
        mi += 1
    return result

def make_vert_group_lookup(obj, bone_lu):
    result = {}
    for vg in obj.vertex_groups:
        n = vg.name
        if n in bone_lu:
            result[vg.index] = n
    return result

def make_bone_lookup(bones):
    for b in bones:
        if b.parent not in bones:
            hier = [b] + [a for a in b.children_recursive if a in bones]
            ilu = dict(zip(hier, range(len(hier))))
            result = {b.name:(0,-1,b)}
            for c in hier[1:]:
                result[c.name] = (ilu[c],ilu[c.parent],c)
            return result

def prep_bmesh(obj, bone_lu):
    tx_lu = make_texture_lookup(obj.material_slots)
    bm = bmesh.new()
    bm.from_object(obj, bpy.context.scene)
    triangulate(strip_wires(bm))
    bm.normal_update()
    v_groups = make_vert_group_lookup(obj, bone_lu)
    dl = bm.verts.layers.deform.verify()
    for v in bm.verts:
        wts = v[dl]
        w_dict = {}
        for k in wts.keys():
            if k in v_groups and wts[k] > 0:
                w_dict[bone_lu[v_groups[k]][0]] = wts[k]
        if not w_dict:
            w_dict = {0:1.0}
        fac = 1.0 / sum(w_dict.values())
        wts.clear()
        for k in w_dict:
            wts[k] = w_dict[k] * fac
    return tx_lu, bm

def get_bones(arm):
    if not arm:
        return []
    all_bones = arm.pose.bones[:]
    if not all_bones:
        return []
    group = None
    for g in arm.pose.bone_groups:
        if g.name.lower() == "smd":
            group = g
            break
    if not group:
        return all_bones
    result = [b for b in all_bones if b.bone_group == group]
    if not result:
        return all_bones
    return result

def message(id, *details):
    if id == 'no_obj':
        return "".join([
            "There is no active mesh in the scene.\n",
            "Select a mesh, and retry export."])
    elif id == 'roots':
        return "".join([
            "There are multiple root bones marked for export.",
            " Either they have no parents",
            " or their parents are excluded from export.\n",
            "Make sure there's a single root or revise the membership",
            " of the \"smd\" bone group, and retry export.\n",
            "Offending bones:\n".join(details[0])])

###
### Export UI
###

class MaybeExportGSSMDMesh(bpy.types.Operator):
    '''Export selection as a reference SMD file'''
    bl_idname = "export_scene.maybe_export_gs_smd_mesh"
    bl_label = 'Export reference SMD'
    def invoke(self, context, event):
        check_result = is_export_go()
        if check_result[0] == 'ok':
            return bpy.ops.export_scene.gs_smd_mesh('INVOKE_DEFAULT')
        else:
            msg_lines = message(check_result[0], check_result[1])
            print(msg_lines)
            self.report({'ERROR'}, msg_lines)
            return {'CANCELLED'}

class MaybeExportGSSMDAnim(bpy.types.Operator):
    '''Export single animation SMD (use current frame range)'''
    bl_idname = "export_scene.maybe_export_gs_smd_anim"
    bl_label = 'Export animation SMD'
    def invoke(self, context, event):
        check_result = is_export_go()
        if check_result[0] == 'ok':
            return bpy.ops.export_scene.gs_smd_anim('INVOKE_DEFAULT')
        else:
            msg_lines = message(check_result[0], check_result[1])
            print(msg_lines)
            self.report({'ERROR'}, msg_lines)
            return {'CANCELLED'}

class MaybeExportGSSMDBatch(bpy.types.Operator):
    '''Export a batch of SMD files'''
    bl_idname = "export_scene.maybe_export_gs_smd_batch"
    bl_label = 'Export SMD Files'
    def invoke(self, context, event):
        check_result = is_export_go()
        if check_result[0] == 'ok':
            return bpy.ops.export_scene.gs_smd_batch('INVOKE_DEFAULT')
        else:
            msg_lines = message(check_result[0], check_result[1])
            print(msg_lines)
            self.report({'ERROR'}, msg_lines)
            return {'CANCELLED'}

class ExportGSSMDMesh(bpy.types.Operator, ExportHelper):
    '''Save a Reference SMD File'''
    bl_idname = "export_scene.gs_smd_mesh"
    bl_label = 'Export SMD'
    bl_options = {'PRESET'}
    filename_ext = ".smd"
    filter_glob = StringProperty(
            default="*.smd",
            options={'HIDDEN'})
    path_mode = path_reference_mode
    check_extension = True
    weighting = BoolProperty(
        name="Include vertex weights",
        description="Supported by Source; not supported by GoldSrc or Xash3D",
        default=False)
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        prerequisites = get_prereqs('mesh')
        write_smd_mesh(self.filepath, prerequisites, self.weighting)
        return {'FINISHED'}

class ExportGSSMDAnim(bpy.types.Operator, ExportHelper):
    '''Save an Animation SMD File'''
    bl_idname = "export_scene.gs_smd_anim"
    bl_label = 'Export SMD'
    bl_options = {'PRESET'}
    filename_ext = ".smd"
    filter_glob = StringProperty(
            default="*.smd",
            options={'HIDDEN'})
    path_mode = path_reference_mode
    check_extension = True
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        prerequisites = get_prereqs('anim')
        write_smd_anim(self.filepath, prerequisites, None)
        return {'FINISHED'}

class ExportGSSMDBatch(bpy.types.Operator, ExportHelper):
    '''Save SMD Files'''
    bl_idname = "export_scene.gs_smd_batch"
    bl_label = 'Export SMD Files'
    bl_options = {'PRESET'}
    filename_ext = ".smd"
    filter_glob = StringProperty(
            default="*.smd",
            options={'HIDDEN'})
    path_mode = path_reference_mode
    check_extension = True
    marker_filter = StringProperty(
            name="Marker filter",
            description="".join([
                "Export only frame ranges tagged with",
                " markers whose names start with this"]),
            default="")
    weighting = BoolProperty(
        name="Include vertex weights",
        description="Supported by Source; not supported by GoldSrc or Xash3D",
        default=False)
    qc = BoolProperty(
        name="Write .qc stub",
        description="The .qc file will be named after your reference .smd",
        default=False)
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        prerequisites = get_prereqs('batch')
        write_batch(
                self.filepath,
                prerequisites,
                self.marker_filter,
                self.weighting,
                self.qc)
        return {'FINISHED'}

def menu_func_export_mesh(self, context):
    self.layout.operator(
        MaybeExportGSSMDMesh.bl_idname,
        text="Studiomodel Mesh Source (.smd)")
def menu_func_export_anim(self, context):
    self.layout.operator(
        MaybeExportGSSMDAnim.bl_idname,
        text="Studiomodel Animation Source (.smd)")
def menu_func_export_batch(self, context):
    self.layout.operator(
        MaybeExportGSSMDBatch.bl_idname,
        text="Studiomodel source files (batch export)")

def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_export.append(menu_func_export_mesh)
    bpy.types.INFO_MT_file_export.append(menu_func_export_anim)
    bpy.types.INFO_MT_file_export.append(menu_func_export_batch)

def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_export.remove(menu_func_export_mesh)
    bpy.types.INFO_MT_file_export.remove(menu_func_export_anim)
    bpy.types.INFO_MT_file_export.remove(menu_func_export_batch)

if __name__ == "__main__":
    register()
