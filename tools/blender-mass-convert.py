import os
import bpy
import re

path_to_import = os.path.join('home', 'brpocock', 'Documents', 'Models to convert')
file_list = sorted(os.listdir(path_to_import))

object_list = [item for item in file_list if item.endswith('.obj')]

obj_pattern = re.compile("\.obj$")
for item in object_list:
    pathname = os.path.join(path_to_import, item)
    bpy.ops.object.mode_set(mode='OBJECT')
    bpy.ops.object.select_all(action='SELECT')
    bpy.ops.object.delete()
    bpy.ops.import_scene.obj(filepath = pathname)
    out_pathname = re.sub(obj_pattern, "", pathname) + '.babylon'
    bpy.ops.scene.babylon(filepath = out_pathname)
