bl_info = {
    "name": "id tech 4 MD5 format",
    "author": "nemyax",
    "version": (0, 8, 20150618),
    "blender": (2, 6, 6),
    "location": "File > Import-Export",
    "description": "Import and export md5mesh and md5anim",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Import-Export"}

import bpy
import os
import bmesh
import os.path
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
    ImportHelper,
    path_reference_mode)

msgLines = [] # global for error messages
prerequisites = None # global for exportable objects
md5Layer = IntProperty(
        name="Bone Layer",
        description="Bone layer reserved for MD5 export",
        min=1, max=20,
        default=5)
bpy.types.Scene.md5_bone_layer = md5Layer

def compat(major, minor, rev):
    v = bpy.app.version
    return v[0] >= major and v[1] >= minor and v[2] >= rev

###
### Import functions
###

### .md5mesh import

def read_md5mesh(path, matrix):
    i = "\s+(\d+)"
    w = "\s+(.+?)"
    a = "(.+?)"
    j_re  = re.compile(
        "\s*\""+a+"\""+w+"\s+\("+w*3+"\s+\)\s+\("+w*3+"\s+\).*")
    v_re  = re.compile("\s*vert"+i+"\s+\("+w*2+"\s+\)"+i*2+".*")
    t_re  = re.compile("\s*tri"+i*4+".*")
    w_re  = re.compile("\s*weight"+i*2+w+"\s+\("+w*3+"\).*")
    e_re  = re.compile("\s*}.*")
    js_re = re.compile("\s*joints\s+{.*")
    n_re  = re.compile("\s*(numverts).*")
    m_re  = re.compile("\s*mesh\s+{.*")
    s_re  = re.compile("\s*shader\s+\""+a+"\".*")
    fh = open(path, "r")
    md5mesh = fh.readlines()
    fh.close()
    m = None
    while not m:
        m = js_re.match(md5mesh.pop(0))
    arm_o, ms = do_joints(md5mesh, j_re, e_re)
    pairs = []
    while md5mesh:
        mat_name, bm = do_mesh(md5mesh, s_re, v_re, t_re, w_re, e_re, n_re, ms)
        pairs.append((mat_name, bm))
        skip_until(m_re, md5mesh)
    for mat_name, bm in pairs:
        mesh = bpy.data.meshes.new("md5mesh")
        bm.to_mesh(mesh)
        bm.free()
        mesh_o = bpy.data.objects.new("md5mesh", mesh)
        vgs = mesh_o.vertex_groups
        for jn, _ in ms:
            vgs.new(name=jn)
        arm_mod = mesh_o.modifiers.new(type='ARMATURE', name="MD5_skeleton")
        arm_mod.object = arm_o
        bpy.context.scene.objects.link(mesh_o)
        bpy.context.scene.objects.active = mesh_o
        bpy.ops.object.mode_set(mode='EDIT')
        bpy.ops.object.material_slot_add()
        try:
            mat = bpy.data.materials[mat_name]
        except KeyError:
            mat = bpy.data.materials.new(mat_name)
        mesh_o.material_slots[-1].material = mat
        bpy.ops.object.mode_set()

def do_mesh(md5mesh, s_re, v_re, t_re, w_re, e_re, n_re, ms):
    bm = bmesh.new()
    mat_name = gather(s_re, n_re, md5mesh)[0][0]
    vs, ts, ws = gather_multi([v_re, t_re, w_re], e_re, md5mesh)
    wd  = bm.verts.layers.deform.verify()
    uvs = bm.loops.layers.uv.verify()
    for vi in range(len(vs)):
        wt, nwt = map(int, vs[vi][3:])
        w0 = ws[wt]
        mtx = ms[int(w0[1])][1]
        xyz = mtx * mu.Vector(map(float, w0[3:]))
        new_v = bm.verts.new(xyz)
        bm.verts.index_update()
        for i in ws[wt:wt+nwt]:
            index = int(i[1])
            val = float(i[2])
            new_v[wd][index] = val
    if compat(2, 73, 0):
        bm.verts.ensure_lookup_table()
    for t in ts:
        tvs = [bm.verts[a] for a in map(int, t[1:])]
        new_f = bm.faces.new(tvs)
        bm.faces.index_update()
        for vn in tvs:
            ln = [l for l in new_f.loops if l.vert == vn][0]
            u0, v0 = map(float, vs[vn.index][1:3])
            ln[uvs].uv = (u0, 1.0 - v0)
    return mat_name, bm

def do_joints(md5mesh, j_re, e_re):
    joints = {}
    jdata = gather(j_re, e_re, md5mesh)
    for i in range(len(jdata)):
        joints[i] = jdata[i]
    arm = bpy.data.armatures.new("MD5")
    arm_o = bpy.data.objects.new("MD5", arm)
    bpy.context.scene.objects.link(arm_o)
    bpy.context.scene.objects.active = arm_o
    bpy.ops.object.mode_set()
    bpy.ops.object.mode_set(mode='EDIT')
    ebs = arm.edit_bones
    ms = []
    for j in joints.values():
        j_name = j[0]
        eb = ebs.new(j_name)
        p = int(j[1])
        if p >= 0:
            eb.parent = ebs[joints[p][0]]
        tx, ty, tz, rx, ry, rz = [float(a) for a in j[2:]]
        quat = -mu.Quaternion(restore_quat(rx, ry, rz))
        mtx = mu.Matrix.Translation((tx, ty, tz)) * quat.to_matrix().to_4x4()
        ms.append((j_name, mtx))
        eb.head = (0.0, 0.0, 0.0)
        eb.tail = (0.0, 1.0, 0.0)
        eb.matrix = mtx
        eb.length = 5.0
    bpy.ops.object.mode_set()
    bl = bpy.context.scene.md5_bone_layer - 1
    for b in arm.bones:
        b.layers[bl] = True
    return arm_o, ms

### .md5anim import functions

def read_md5anim(path):
    label = path.split(os.sep)[-1].split(".")[-2]
    ao = bpy.context.active_object
    skel = bone_tree_blender(ao.data)
    fh = open(path, "r")
    md5anim = fh.readlines()
    fh.close()
    w = "\s+(.+?)"
    a = "(.+?)"
    j_re   = re.compile("\s*\""+a+"\""+w*3+".*")
    e_re   = re.compile("\s*}.*")
    bf0_re = re.compile("\s*(baseframe)\s+{.*")
    bf1_re = re.compile("\s*\("+w*3+"\s+\)\s+\("+w*3+"\s+\).*")
    f_re   = re.compile("\s*(frame).*")
    hier = gather(j_re, e_re, md5anim)
    for i in range(len(hier)):
        jname, par, flags = hier[i][:-1]
        hier[i] = (i, jname, int(par), int(flags))
    md5skel = bone_tree_md5(hier)
    if skel != md5skel:
        return message("no_arm_match"), {'CANCELLED'}
    skip_until(bf0_re, md5anim)
    bframe = gather(bf1_re, e_re, md5anim)
    bxfs = get_base_xforms(bframe)
    skip_until(f_re, md5anim)
    pbs = [ao.pose.bones[j[1]] for j in hier]
    frames = pad_frames(hier, bxfs, do_frames(e_re, md5anim))
    fcurves = get_fcurves(pbs)
    xf_keys = convert_xforms(pbs, transpose(frames))
    sc = bpy.context.scene
    start_frame = sc.frame_current
    end_frame = start_frame + len(frames) - 1
    set_keys(
        flatten_channels(fcurves),
        flatten_frames(xf_keys),
        start_frame)
    sc.timeline_markers.new(label + "_start", start_frame)
    sc.timeline_markers.new(label + "_end", end_frame)
    sc.frame_current = end_frame
    return "Animation imported successfully.", {'FINISHED'}

def do_frames(e_re, md5anim):
    valid = re.compile("[-\.0-9\s]+")
    val   = re.compile("(\S+)")
    result = [[]]
    while md5anim:
        l = md5anim.pop(0)
        if e_re.match(l):
            result.append([])
            continue
        if valid.match(l):
            vals = [float(a) for a in val.findall(l)]
            if vals:
                result[-1].append(vals)
    return [a for a in result if a]

def get_base_xforms(bframe):
    return [[float(b) for b in a] for a in bframe]

def convert_xforms(pbs, val_lists):
    result = []
    for pb, states in zip(pbs, val_lists):
        result.append([])
        par = pb.parent
        if par:
            tweak = pb.bone.matrix_local.inverted() * par.bone.matrix_local
        else:
            tweak = pb.bone.matrix_local.inverted()
        for vl in states:
            l = mu.Vector(vl[:3])
            q = mu.Quaternion(restore_quat(vl[3], vl[4], vl[5]))
            mtx = q.to_matrix().to_4x4()
            mtx.translation = l
            mtx = tweak * mtx
            xf = []
            xf.extend(mtx.translation[:])
            xf.extend(mtx.to_quaternion()[:])
            xf.extend(mtx.to_euler()[:])
            result[-1].append(xf)
    return result

def pad_frames(hier, bxfs, frames):
    result = []
    for val_lists in frames:
        result.append([])
        for j, xf in zip(hier, bxfs):
            xf0 = xf[:]
            flags = j[3]
            if not flags:
                vl = []
            else:
                vl = val_lists.pop(0)
            mask = 1
            for i in range(6):
                if mask & flags:
                    xf0[i] = vl.pop(0)
                mask *= 2
            result[-1].append(xf0)
    return result

def transpose(table):
    result = []
    if table:
        while table[0]:
            result.append([])
            for col in table:
                result[-1].append(col.pop(0))
    return result

def get_fcurves(pbs):
    cf = float(bpy.context.scene.frame_current)
    fcurves = []
    l = "location"
    q = "rotation_quaternion"
    e = "rotation_euler"
    for pb in pbs:
        pb.keyframe_insert(l)
        pb.keyframe_insert(q)
        pb.keyframe_insert(e)
        entry = {l:{},q:{},e:{}}
        pbn = pb.name
        fc_re = re.compile("pose\.bones\[."+pbn+".\]\.("+l+"|"+q+"|"+e+")")
        for fc in pb.id_data.animation_data.action.fcurves:
            m = fc_re.match(fc.data_path)
            if m:
                key1 = m.group(1)
                key2 = fc.array_index
                entry[key1][key2] = fc
        fcurves.append(entry)
    return fcurves

def list_fcurves(fcurves):
    l = "location"
    q = "rotation_quaternion"
    e = "rotation_euler"
    return [
        fcurves[l][0], fcurves[l][1], fcurves[l][2],
        fcurves[q][0], fcurves[q][1], fcurves[q][2], fcurves[q][3],
        fcurves[e][0], fcurves[e][1], fcurves[e][2]]

def flatten_channels(fcurves):
    result = []
    for a in fcurves:
        result.extend([b.keyframe_points for b in list_fcurves(a)])
    return result

def flatten_frames(pbs): # :: [[[a]]] -> [[a]]
    result = []
    for b in pbs:
        temp = [[] for _ in range(10)]
        for frame in b:
            for i in range(10):
                temp[i].append(frame[i])
        result.extend(temp)
    return result

def set_keys(channels, val_lists, f_start):
    for ch, vl in zip(channels, val_lists):
        i = f_start
        for v in vl:
            ch.insert(i, v)
            i += 1

### parsing and utility functions

def gather(regex, end_regex, ls):
    return gather_multi([regex], end_regex, ls)[0]
 
def gather_multi(regexes, end_regex, ls):
    result = [[] for _ in regexes]
    n = len(regexes)
    while ls:
        l = ls.pop(0)
        if end_regex.match(l):
            break
        for i in range(n):
            m = regexes[i].match(l)
            if m:
                result[i].append(m.groups())
                break
    return result

def skip_until(regex, ls):
    while ls:
        if regex.match(ls.pop(0)):
            break

def restore_quat(rx, ry, rz):
    t = 1.0 - (rx * rx) - (ry * ry) - (rz * rz)
    if t < 0.0:
        return (0.0, rx, ry, rz)
    else:
        return (-math.sqrt(t), rx, ry, rz)

def bone_tree_blender(arm):
    bl = bpy.context.scene.md5_bone_layer - 1
    return btb(None, [b for b in arm.bones if b.layers[bl]])

def btb(b, bs): # recursive; shouldn't matter for poxy md5 skeletons
    ch = sorted([a for a in bs if a.parent == b], key=lambda x: x.name)
    return [[c.name, btb(c, bs)] for c in ch]

def bone_tree_md5(lst):
    root = [a for a in lst if a[2] == -1][0]
    return [[root[1], btm(root, lst)]]

def btm(e, l):
    ch = sorted([a for a in l if a[2] == e[0]], key=lambda x: x[1])
    return [[c[1], btm(c, l)] for c in ch]

###
### Export functions
###

def get_ranges(markerFilter):
    markers = bpy.context.scene.timeline_markers
    starts = [m for m in markers if
        m.name.startswith(markerFilter) and
        m.name.endswith("_start", 2)]
    ends = [m for m in markers if
        m.name.startswith(markerFilter) and
        m.name.endswith("_end", 2)]
    if not starts or not ends:
        return None
    else:
        return find_matches(starts, ends)
    
def find_matches(starts, ends):
    pairs = {}
    for s in starts:
        basename = s.name[:s.name.rfind("_start")]
        matches = [e for e in ends if
            e.name[:e.name.rfind("_end")] == basename]
        if matches:
            m = matches[0]
            pairs[basename] = (min(s.frame, m.frame), max(s.frame, m.frame))
    return pairs

def record_parameters(correctionMatrix):
    return "".join([
        " // Parameters used during export:",
        " Reorient: {};".format(bool(correctionMatrix.to_euler()[2])),
        " Scale: {}".format(correctionMatrix.decompose()[2][0])])

def define_components(obj, bm, bones, correctionMatrix):
    scaleFactor = correctionMatrix.to_scale()[0]
    armature = [a for a in bpy.data.armatures if bones[0] in a.bones[:]][0]
    armatureObj = [o for o in bpy.data.objects if o.data == armature][0]
    boneNames = [b.name for b in bones]
    allVertGroups = obj.vertex_groups[:]
    weightGroupIndexes = [vg.index for vg in allVertGroups if vg.name in boneNames]
    uvData = bm.loops.layers.uv.active
    weightData = bm.verts.layers.deform.active
    tris = [[f.index, f.verts[2].index, f.verts[1].index, f.verts[0].index]
        for f in bm.faces] # reverse vert order to flip normal
    verts = []
    weights = []
    wtIndex = 0
    firstWt = 0
    for vert in bm.verts:
        vGroupDict = vert[weightData]
        wtDict = dict([(k, vGroupDict[k]) for k in vGroupDict.keys()
            if k in weightGroupIndexes])
        u = vert.link_loops[0][uvData].uv.x
        v = 1 - vert.link_loops[0][uvData].uv.y # MD5 wants it flipped
        numWts = len(wtDict.keys())
        verts.append([vert.index, u, v, firstWt, numWts])
        wtScaleFactor = 1.0 / sum(wtDict.values())
        firstWt += numWts
        for vGroup in wtDict:
            bone = [b for b in bones
                if b.name == allVertGroups[vGroup].name][0]
            boneIndex = bones.index(bone)
            coords4d =\
                bone.matrix_local.inverted() *\
                armatureObj.matrix_world.inverted() *\
                obj.matrix_world *\
                (vert.co.to_4d() * scaleFactor)
            x, y, z = coords4d[:3]
            weight = wtDict[vGroup] * wtScaleFactor
            wtEntry = [wtIndex, boneIndex, weight, x, y, z]
            weights.append(wtEntry)
            wtIndex += 1
    return (verts, tris, weights)

def make_hierarchy_block(bones, boneIndexLookup):
    block = ["hierarchy {\n"]
    xformIndex = 0
    for b in bones:
        if b.parent:
            parentIndex = boneIndexLookup[b.parent.name]
        else:
            parentIndex = -1
        block.append("  \"{}\" {} 63 {} //\n".format(
            b.name, parentIndex, xformIndex))
        xformIndex += 6
    block.append("}\n")
    block.append("\n")
    return block

def make_baseframe_block(bones, correctionMatrix):
    block = ["baseframe {\n"]
    armature = bones[0].id_data
    armObject = [o for o in bpy.data.objects
        if o.data == armature][0]
    armMatrix = armObject.matrix_world
    for b in bones:
        objSpaceMatrix = b.matrix_local
        if b.parent:
            bMatrix =\
            b.parent.matrix_local.inverted() *\
            armMatrix *\
            objSpaceMatrix
        else:
            bMatrix = correctionMatrix * objSpaceMatrix
        xPos, yPos, zPos = bMatrix.translation
        xOrient, yOrient, zOrient = (-bMatrix.to_quaternion()).normalized()[1:]
        block.append("  ( {:.10f} {:.10f} {:.10f} ) ( {:.10f} {:.10f} {:.10f} )\n".\
        format(xPos, yPos, zPos, xOrient, yOrient, zOrient))
    block.append("}\n")
    block.append("\n")
    return block

def make_joints_block(bones, boneIndexLookup, correctionMatrix):
    block = []
    block.append("joints {\n")
    for b in bones:
        if b.parent:
            parentIndex = boneIndexLookup[b.parent.name]
        else:
            parentIndex = -1
        boneMatrix = correctionMatrix * b.matrix_local
        xPos, yPos, zPos = boneMatrix.translation
        xOrient, yOrient, zOrient =\
        (-boneMatrix.to_quaternion()).normalized()[1:] # MD5 wants it negated
        block.append(\
        "  \"{}\" {} ( {:.10f} {:.10f} {:.10f} ) ( {:.10f} {:.10f} {:.10f} )\n".\
        format(b.name, parentIndex,\
        xPos, yPos, zPos,\
        xOrient, yOrient, zOrient))
    block.append("}\n")
    block.append("\n")
    return block

def make_mesh_block(obj, bones, correctionMatrix):
    shaderName = "default"
    ms = obj.material_slots
    if ms:
        taken = [s for s in ms if s.material]
        if taken:
            shaderName = taken[0].material.name
    bm = bmesh.new()
    bm.from_mesh(obj.data)
    triangulate(cut_up(strip_wires(bm)))
    verts, tris, weights = define_components(obj, bm, bones, correctionMatrix)
    bm.free()
    block = []
    block.append("mesh {\n")
    block.append("  shader \"{}\"\n".format(shaderName))
    block.append("  numverts {}\n".format(len(verts)))
    for v in verts:
        block.append(\
        "  vert {} ( {:.10f} {:.10f} ) {} {}\n".\
        format(v[0], v[1], v[2], v[3], v[4]))
    block.append("  numtris {}\n".format(len(tris)))
    for t in tris:
        block.append("  tri {} {} {} {}\n".format(t[0], t[1], t[2], t[3]))
    block.append("  numweights {}\n".format(len(weights)))
    for w in weights:
        block.append(\
        "  weight {} {} {:.10f} ( {:.10f} {:.10f} {:.10f} )\n".\
        format(w[0], w[1], w[2], w[3], w[4], w[5]))
    block.append("}\n")
    block.append("\n")
    return block

def strip_wires(bm):
    [bm.faces.remove(f) for f in bm.faces if len(f.verts) < 3]
    [bm.edges.remove(e) for e in bm.edges if not e.link_faces[:]]
    [bm.verts.remove(v) for v in bm.verts if v.is_wire]
    for seq in [bm.verts, bm.faces, bm.edges]: seq.index_update()
    return bm

def cut_up(bm):
    uvData = bm.loops.layers.uv.active
    for v in bm.verts:
        for e in v.link_edges:
            linkedFaces = e.link_faces
            if len(linkedFaces) > 1:
                uvSets = []
                for lf in linkedFaces:
                    uvSets.append([l1[uvData].uv for l1 in lf.loops
                        if l1.vert == v][0])
                if uvSets.count(uvSets[0]) != len(uvSets):
                    e.tag = True
                    v.tag = True
        if v.tag:
            seams = [e for e in v.link_edges if e.tag]
            v.tag = False
            bmesh.utils.vert_separate(v, seams)
    for maybeBowTie in bm.verts: # seems there's no point in a proper test
        boundaries = [e for e in maybeBowTie.link_edges
            if len(e.link_faces) == 1]
        bmesh.utils.vert_separate(maybeBowTie, boundaries)
    for seq in [bm.verts, bm.faces, bm.edges]: seq.index_update()
    return bm

def triangulate(bm):
    nonTris = [f for f in bm.faces if len(f.verts) > 3]
    bmesh.ops.triangulate(bm, faces=nonTris)
    return bm

def write_md5mesh(filePath, prerequisites, correctionMatrix):
    bones, meshObjects = prerequisites
    boneIndexLookup = {}
    for b in bones:
        boneIndexLookup[b.name] = bones.index(b)
    md5joints = make_joints_block(bones, boneIndexLookup, correctionMatrix)
    md5meshes = []
    for mo in meshObjects:
        md5meshes.append(make_mesh_block(mo, bones, correctionMatrix))
    f = open(filePath, 'w')
    lines = []
    lines.append("MD5Version 10" + record_parameters(correctionMatrix) + "\n")
    lines.append("commandline \"\"\n")
    lines.append("\n")
    lines.append("numJoints " + str(len(bones)) + "\n")
    lines.append("numMeshes " + str(len(meshObjects)) + "\n")
    lines.append("\n")
    lines.extend(md5joints)
    for m in md5meshes: lines.extend(m)
    for line in lines: f.write(line)
    f.close()
    return

def write_md5anim(filePath, prerequisites, correctionMatrix, frameRange):
    goBack = bpy.context.scene.frame_current
    if frameRange == None:
        startFrame = bpy.context.scene.frame_start
        endFrame = bpy.context.scene.frame_end
    else:
        startFrame, endFrame = frameRange
    bones, meshObjects = prerequisites
    armObj = [o for o in bpy.data.objects if o.data == bones[0].id_data][0]
    pBones = armObj.pose.bones
    boneIndexLookup = {}
    for b in bones:
        boneIndexLookup[b.name] = bones.index(b)
    hierarchy = make_hierarchy_block(bones, boneIndexLookup)
    baseframe = make_baseframe_block(bones, correctionMatrix)
    bounds = []
    frames = []
    for frame in range(startFrame, endFrame + 1):
        bpy.context.scene.frame_set(frame)
        verts = []
        for mo in meshObjects:
            bm = bmesh.new()
            bm.from_object(mo, bpy.context.scene)
            verts.extend([correctionMatrix * mo.matrix_world * v.co.to_4d()
                for v in bm.verts])
            bm.free()
        minX = min([co[0] for co in verts])
        minY = min([co[1] for co in verts])
        minZ = min([co[2] for co in verts])
        maxX = max([co[0] for co in verts])
        maxY = max([co[1] for co in verts])
        maxZ = max([co[2] for co in verts])
        bounds.append(\
        "  ( {:.10f} {:.10f} {:.10f} ) ( {:.10f} {:.10f} {:.10f} )\n".\
        format(minX, minY, minZ, maxX, maxY, maxZ))
        frameBlock = ["frame {} {{\n".format(frame - startFrame)]
        scaleFactor = correctionMatrix.to_scale()[0]
        for b in bones:
            pBone = pBones[b.name]
            pBoneMatrix = pBone.matrix
            if pBone.parent:
                diffMatrix = pBone.parent.matrix.inverted() * armObj.matrix_world * (pBoneMatrix * scaleFactor)
            else:
                diffMatrix = correctionMatrix * pBoneMatrix
            xPos, yPos, zPos = diffMatrix.translation
            xOrient, yOrient, zOrient =\
            (-diffMatrix.to_quaternion()).normalized()[1:]
            frameBlock.append(\
            "  {:.10f} {:.10f} {:.10f} {:.10f} {:.10f} {:.10f}\n".\
            format(xPos, yPos, zPos, xOrient, yOrient, zOrient))
        frameBlock.append("}\n")
        frameBlock.append("\n")
        frames.extend(frameBlock)
    f = open(filePath, 'w')
    numJoints = len(bones)
    bounds.insert(0, "bounds {\n")
    bounds.append("}\n")
    bounds.append("\n")
    lines = []
    lines.append("MD5Version 10" + record_parameters(correctionMatrix) + "\n")
    lines.append("commandline \"\"\n")
    lines.append("\n")
    lines.append("numFrames " + str(endFrame - startFrame + 1) + "\n")
    lines.append("numJoints " + str(numJoints) + "\n")
    lines.append("frameRate " + str(bpy.context.scene.render.fps) + "\n")
    lines.append("numAnimatedComponents " + str(numJoints * 6) + "\n")
    lines.append("\n")
    for chunk in [hierarchy, bounds, baseframe, frames]:
        lines.extend(chunk)
    for line in lines:
        f.write(line)
    bpy.context.scene.frame_set(goBack)
    return

def write_batch(filePath, prerequisites, correctionMatrix, markerFilter):
    write_md5mesh(filePath, prerequisites, correctionMatrix)
    ranges = get_ranges(markerFilter)
    if ranges:
        for r in ranges.keys():
            folder = os.path.dirname(filePath)
            animFile = os.path.join(folder, r + ".md5anim")
            write_md5anim(
                animFile, prerequisites, correctionMatrix, ranges[r])
        return {'FINISHED'}
    else:
        baseFilePathEnd = filePath.rfind(".md5mesh")
        if baseFilePathEnd == -1:
            animFilePath = filePath + ".md5anim"
        else:
            animFilePath = filePath[:baseFilePathEnd] + ".md5anim"
        write_md5anim(animFilePath, prerequisites, correctionMatrix, None)
        return {'FINISHED'}

###
### Operators and auxiliary functions
###

# Functions

def concat_strings(strings):
    result = ""
    for s in strings:
        result = result + "\n" + s
    return result

def message(id, *details):
    if id == 'no_deformables':
        return """No armature-deformed meshes are selected.
Select the meshes you want to export, and retry export."""
    elif id == 'multiple_armatures':
        return """The selected meshes use more than one armature.
Select meshes that use the same armature, and try again."""
    elif id == 'no_armature':
        return """No deforming armature is associated with the selection.
Select the model or models you want to export, and try again"""
    elif id == 'layer_empty':
        bl = str(bpy.context.scene.md5_bone_layer)
        return "The deforming armature has no bones in layer " + bl + """.
Add all of the bones you want to export to the armature's layer """ +\
        bl + """,
or change the reserved bone layer in the scene properties,
and retry export."""
    elif id == 'missing_parents':
        bl = str(bpy.context.scene.md5_bone_layer)
        return "One or more bones have parents outside layer " + bl + """.
Revise your armature's layer """ + bl + """ membership,
or change the reserved bone layer in the scene properties, and retry export.
Offending bones:""" + concat_strings(details[0])
    elif id == 'orphans':
        bl = str(bpy.context.scene.md5_bone_layer)
        return """There are multiple root bones (listed below)
in the export-bound collection, but only one root bone
is allowed in MD5. Revise your armature's layer """ + bl + """ membership,
or change the reserved bone layer in the scene properties, and retry export.
Root bones:""" + concat_strings(details[0])
    elif id == 'unweighted_verts':
        if details[0][1] == 1:
            count = " 1 vertex "
        else:
            count = " " + str(details[0][1]) + " vertices "
        return "The '" + details[0][0] + "' object contains" + count +\
        """with no deformation weights assigned.
Valid MD5 data cannot be produced. Paint non-zero weights
on all the vertices in the mesh, and retry export."""
    elif id == 'zero_weight_verts':
        if details[0][1] == 1:
            count = " 1 vertex "
        else:
            count = " " + str(details[0][1]) + " vertices "
        return "The '" + details[0][0] + "' object contains" + count +\
        """with zero weights assigned.
This can cause adverse effects.
Paint non-zero weights on all the vertices in the mesh,
or use the Clean operation in the weight paint tools,
and retry export."""
    elif id == 'no_uvs':
        return "The '" + details[0] + """' object has no UV coordinates.
Valid MD5 data cannot be produced. Unwrap the object
or exclude it from your selection, and retry export."""
    elif id == 'no_arm':
        return """No armature is selected to add animation to.
Select a valid armature, and retry import."""
    elif id == 'no_arm_match':
        return """The selected armature does not match the skeleton
in the file you are trying to import."""

def check_weighting(obj, bm, bones):
    boneNames = [b.name for b in bones]
    allVertGroups = obj.vertex_groups[:]
    weightGroups = [vg for vg in allVertGroups if vg.name in boneNames]
    weightGroupIndexes = [vg.index for vg in allVertGroups if vg.name in boneNames]
    weightData = bm.verts.layers.deform.active
    unweightedVerts = 0
    zeroWeightVerts = 0
    for v in bm.verts:
        influences = [wgi for wgi in weightGroupIndexes
            if wgi in v[weightData].keys()]
        if not influences:
            unweightedVerts += 1
        else:
            for wgi in influences:
                if v[weightData][wgi] < 0.000001:
                    zeroWeightVerts += 1
    return (unweightedVerts, zeroWeightVerts)

def is_export_go(what, selection):
    bl = bpy.context.scene.md5_bone_layer - 1
    meshObjects = [o for o in selection
        if o.data in bpy.data.meshes[:] and o.find_armature()]
    armatures = [a.find_armature() for a in meshObjects]
    if not meshObjects:
        return ['no_deformables', None]
    armature = armatures[0]
    if armatures.count(armature) < len(meshObjects):
        return ['multiple_armatures', None]
    bones = [b for b in armature.data.bones if b.layers[bl]]
    if not bones:
        return ['layer_empty', None]
    rootBones = [i for i in bones if not i.parent]
    if len(rootBones) > 1:
        boneList = []
        for rb in rootBones:
            boneList.append("- " + str(rb.name))
        return ['orphans', boneList]
    abandonedBones = [i for i in bones
        if i.parent and i.parent not in bones[:]]
    if abandonedBones:
        boneList = []
        for ab in abandonedBones:
            boneList.append("- " + str(ab.name))
        return ['missing_parents', boneList]
    if what != 'anim':
        for mo in meshObjects:
            bm = bmesh.new()
            bm.from_mesh(mo.data)
            (unweightedVerts, zeroWeightVerts) = check_weighting(mo, bm, bones)
            uvLayer = bm.loops.layers.uv.active
            bm.free()
            if unweightedVerts > 0:
                return ['unweighted_verts', (mo.name, unweightedVerts)]
            if zeroWeightVerts > 0:
                return ['zero_weight_verts', (mo.name, zeroWeightVerts)]
            if not uvLayer:
                return ['no_uvs', mo.name]
    return ['ok', (bones, meshObjects)]

def manage_bone_layers(doWhat):
    bl = bpy.context.scene.md5_bone_layer - 1
    mode = bpy.context.mode
    if mode == 'POSE':
        allBones = [pb.bone for pb in bpy.context.active_object.pose.bones]
        selBones = [pb.bone for pb in bpy.context.selected_pose_bones]
    elif mode == 'EDIT_ARMATURE':
        allBones = bpy.context.active_object.data.edit_bones
        selBones = bpy.context.selected_editable_bones
    else:
        return
    unselBones = [b for b in allBones if b not in selBones]
    if doWhat == 'replace':
        for x in selBones:
            x.layers[bl] = True
        for y in unselBones:
            y.layers[bl] = False
        return
    elif doWhat == 'add':
        for x in selBones:
            x.layers[bl] = True
        return
    elif doWhat == 'remove':
        for x in selBones:
            x.layers[bl] = False
        return
    elif doWhat == 'clear':
        for x in allBones:
            x.layers[bl] = False
        return
    else: return

# Operators

### Import UI

class ImportMD5Mesh(bpy.types.Operator, ImportHelper):
    '''Load an MD5 Mesh File'''
    bl_idname = "import_scene.md5mesh"
    bl_label = 'Import MD5MESH'
    bl_options = {'PRESET'}
    filename_ext = ".md5mesh"
    filter_glob = StringProperty(
            default="*.md5mesh",
            options={'HIDDEN'})
    path_mode = path_reference_mode
    check_extension = True
    reorient = BoolProperty(
            name="Reorient",
            description="Change the direction that the model faces",
            default=True)
    scaleFactor = FloatProperty(
            name="Scale",
            description="Scale all data",
            min=0.01, max=1000.0,
            soft_min=0.01,
            soft_max=1000.0,
            default=1.0)
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        orientationTweak = mu.Matrix.Rotation(math.radians(
            -90 * float(self.reorient)),4,'Z')
        scaleTweak = mu.Matrix.Scale(self.scaleFactor, 4)
        correctionMatrix = orientationTweak * scaleTweak
        read_md5mesh(self.filepath, correctionMatrix)
        return {'FINISHED'}

class MaybeImportMD5Anim(bpy.types.Operator):
    '''Import single MD5 animation (start from current frame)'''
    bl_idname = "export_scene.maybe_import_md5anim"
    bl_label = 'Import MD5ANIM'
    def invoke(self, context, event):
        ao = bpy.context.active_object
        if ao and ao.type == 'ARMATURE' and ao.data.bones[:]:
            return bpy.ops.import_scene.md5anim('INVOKE_DEFAULT')
        else:
            msg = message("no_arm")
            print(msg)
            self.report({'ERROR'}, msg)
            return {'CANCELLED'}

class ImportMD5Anim(bpy.types.Operator, ImportHelper):
    '''Load an MD5 Animation File'''
    bl_idname = "import_scene.md5anim"
    bl_label = 'Import MD5ANIM'
    bl_options = {'PRESET'}
    filename_ext = ".md5anim"
    filter_glob = StringProperty(
            default="*.md5anim",
            options={'HIDDEN'})
    path_mode = path_reference_mode
    check_extension = True
    check_extension = True
    def execute(self, context):
        msg, res = read_md5anim(self.filepath)
        if res == {'CANCELLED'}:
            self.report({'ERROR'}, msg)
        print(msg)
        return res

### Bone layer management

class MD5BonesAdd(bpy.types.Operator):
    '''Add the selected bones to the bone layer reserved for MD5'''
    bl_idname = "scene.md5_bones_add"
    bl_label = 'Add Selected'
    def invoke(self, context, event):
        manage_bone_layers('add')
        return {'FINISHED'}

class MD5BonesRemove(bpy.types.Operator):
    '''Remove the selected bones from the bone layer reserved for MD5'''
    bl_idname = "scene.md5_bones_remove"
    bl_label = 'Remove Selected'
    def invoke(self, context, event):
        manage_bone_layers('remove')
        return {'FINISHED'}

class MD5BonesReplace(bpy.types.Operator):
    '''Include only the selected bones in the bone layer reserved for MD5'''
    bl_idname = "scene.md5_bones_replace"
    bl_label = 'Replace with Selected'
    def invoke(self, context, event):
        manage_bone_layers('replace')
        return {'FINISHED'}

class MD5BonesClear(bpy.types.Operator):
    '''Clear the bone layer reserved for MD5'''
    bl_idname = "scene.md5_bones_clear"
    bl_label = 'Clear All'
    def invoke(self, context, event):
        manage_bone_layers('clear')
        return {'FINISHED'}

class MD5Panel(bpy.types.Panel):
    """MD5 parameters panel in the scene context of the properties editor"""
    bl_label = "MD5 Export Setup"
    bl_idname = "SCENE_PT_md5"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "scene"
    def draw(self, context):
        layout = self.layout
        scene = context.scene
        bl = str(scene.md5_bone_layer)
        layout.prop(scene, "md5_bone_layer")
        column1 = layout.column()
        column1.label("Manage layer " + bl + " membership:")
        column2 = column1.column(align=True)
        column2.operator("scene.md5_bones_add")
        column2.operator("scene.md5_bones_remove")
        column2.operator("scene.md5_bones_replace")
        column2.operator("scene.md5_bones_clear")
        if context.mode in {'POSE','EDIT_ARMATURE'}:
            column1.enabled = True
        else:
            column1.enabled = False

### Export UI

class MaybeExportMD5Mesh(bpy.types.Operator):
    '''Export selection as MD5 mesh'''
    bl_idname = "export_scene.maybe_export_md5mesh"
    bl_label = 'Export MD5MESH'
    def invoke(self, context, event):
        global msgLines, prerequisites
        selection = context.selected_objects
        checkResult = is_export_go('mesh', selection)
        if checkResult[0] == 'ok':
            prerequisites = checkResult[-1]
            return bpy.ops.export_scene.md5mesh('INVOKE_DEFAULT')
        else:
            msgLines = message(checkResult[0], checkResult[1])
            print(msgLines)
            self.report({'ERROR'}, msgLines)
            return {'CANCELLED'}

class MaybeExportMD5Anim(bpy.types.Operator):
    '''Export single MD5 animation (use current frame range)'''
    bl_idname = "export_scene.maybe_export_md5anim"
    bl_label = 'Export MD5ANIM'
    def invoke(self, context, event):
        global msgLines, prerequisites
        selection = context.selected_objects
        checkResult = is_export_go('anim', selection)
        if checkResult[0] == 'ok':
            prerequisites = checkResult[-1]
            return bpy.ops.export_scene.md5anim('INVOKE_DEFAULT')
        else:
            msgLines = message(checkResult[0], checkResult[1])
            print(msgLines)
            self.report({'ERROR'}, msgLines)
            return {'CANCELLED'}

class MaybeExportMD5Batch(bpy.types.Operator):
    '''Export a batch of MD5 files'''
    bl_idname = "export_scene.maybe_export_md5batch"
    bl_label = 'Export MD5 Files'
    def invoke(self, context, event):
        global msgLines, prerequisites
        selection = context.selected_objects
        checkResult = is_export_go('batch', selection)
        if checkResult[0] == 'ok':
            prerequisites = checkResult[-1]
            return bpy.ops.export_scene.md5batch('INVOKE_DEFAULT')
        else:
            msgLines = message(checkResult[0], checkResult[1])
            print(msgLines)
            self.report({'ERROR'}, msgLines)
            return {'CANCELLED'}

class ExportMD5Mesh(bpy.types.Operator, ExportHelper):
    '''Save an MD5 Mesh File'''
    global prerequisites
    bl_idname = "export_scene.md5mesh"
    bl_label = 'Export MD5MESH'
    bl_options = {'PRESET'}
    filename_ext = ".md5mesh"
    filter_glob = StringProperty(
            default="*.md5mesh",
            options={'HIDDEN'},
            )
    path_mode = path_reference_mode
    check_extension = True
    reorient = BoolProperty(
            name="Reorient",
            description="Treat +X as the forward direction",
            default=True,
            )
    scaleFactor = FloatProperty(
            name="Scale",
            description="Scale all data",
            min=0.01, max=1000.0,
            soft_min=0.01,
            soft_max=1000.0,
            default=1.0,
            )
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        orientationTweak = mu.Matrix.Rotation(math.radians(
            -90 * float(self.reorient)),4,'Z')
        scaleTweak = mu.Matrix.Scale(self.scaleFactor, 4)
        correctionMatrix = orientationTweak * scaleTweak
        write_md5mesh(self.filepath, prerequisites, correctionMatrix)
        return {'FINISHED'}

class ExportMD5Anim(bpy.types.Operator, ExportHelper):
    '''Save an MD5 Animation File'''
    global prerequisites
    bl_idname = "export_scene.md5anim"
    bl_label = 'Export MD5ANIM'
    bl_options = {'PRESET'}
    filename_ext = ".md5anim"
    filter_glob = StringProperty(
            default="*.md5anim",
            options={'HIDDEN'},
            )
    path_mode = path_reference_mode
    check_extension = True
    reorient = BoolProperty(
            name="Reorient",
            description="Treat +X as the forward direction",
            default=True,
            )
    scaleFactor = FloatProperty(
            name="Scale",
            description="Scale all data",
            min=0.01, max=1000.0,
            soft_min=0.01,
            soft_max=1000.0,
            default=1.0,
            )
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        orientationTweak = mu.Matrix.Rotation(math.radians(
            -90 * float(self.reorient)),4,'Z')
        scaleTweak = mu.Matrix.Scale(self.scaleFactor, 4)
        correctionMatrix = orientationTweak * scaleTweak
        write_md5anim(self.filepath, prerequisites, correctionMatrix, None)
        return {'FINISHED'}

class ExportMD5Batch(bpy.types.Operator, ExportHelper):
    '''Save MD5 Files'''
    global prerequisites
    bl_idname = "export_scene.md5batch"
    bl_label = 'Export MD5 Files'
    bl_options = {'PRESET'}
    filename_ext = ".md5mesh"
    filter_glob = StringProperty(
            default="*.md5mesh",
            options={'HIDDEN'},
            )
    path_mode = path_reference_mode
    check_extension = True
    markerFilter = StringProperty(
            name="Marker filter",
            description="Export only frame ranges tagged with "\
            + "markers whose names start with this",
            default="",
            )
    reorient = BoolProperty(
            name="Reorient",
            description="Treat +X as the forward direction",
            default=True,
            )
    scaleFactor = FloatProperty(
            name="Scale",
            description="Scale all data",
            min=0.01, max=1000.0,
            soft_min=0.01,
            soft_max=1000.0,
            default=1.0,
            )
    path_mode = path_reference_mode
    check_extension = True
    def execute(self, context):
        orientationTweak = mu.Matrix.Rotation(math.radians(
            -90 * float(self.reorient)),4,'Z')
        scaleTweak = mu.Matrix.Scale(self.scaleFactor, 4)
        correctionMatrix = orientationTweak * scaleTweak
        write_batch(
                self.filepath,
                prerequisites,
                correctionMatrix,
                self.markerFilter)
        return {'FINISHED'}

def menu_func_import_mesh(self, context):
    self.layout.operator(
        ImportMD5Mesh.bl_idname, text="MD5 Mesh (.md5mesh)")
def menu_func_import_anim(self, context):
    self.layout.operator(
        MaybeImportMD5Anim.bl_idname, text="MD5 Animation (.md5anim)")

def menu_func_export_mesh(self, context):
    self.layout.operator(
        MaybeExportMD5Mesh.bl_idname, text="MD5 Mesh (.md5mesh)")
def menu_func_export_anim(self, context):
    self.layout.operator(
        MaybeExportMD5Anim.bl_idname, text="MD5 Animation (.md5anim)")
def menu_func_export_batch(self, context):
    self.layout.operator(
        MaybeExportMD5Batch.bl_idname, text="MD5 (batch export)")

def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_import.append(menu_func_import_mesh)
    bpy.types.INFO_MT_file_import.append(menu_func_import_anim)
    bpy.types.INFO_MT_file_export.append(menu_func_export_mesh)
    bpy.types.INFO_MT_file_export.append(menu_func_export_anim)
    bpy.types.INFO_MT_file_export.append(menu_func_export_batch)

def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_import.remove(menu_func_import_mesh)
    bpy.types.INFO_MT_file_import.remove(menu_func_import_anim)
    bpy.types.INFO_MT_file_export.remove(menu_func_export_mesh)
    bpy.types.INFO_MT_file_export.remove(menu_func_export_anim)
    bpy.types.INFO_MT_file_export.remove(menu_func_export_batch)

if __name__ == "__main__":
    register()
