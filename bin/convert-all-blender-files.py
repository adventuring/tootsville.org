import bpy
import os

path = '/home/brpocock/Projects/jumbo.tootsville.org/Assets/Models/5/'  # set this path

for root, dirs, files in os.walk(path):
    for f in files:
        if f.endswith('.obj') :
            obj_file = os.path.join(path, f)
            glb_file = os.path.splitext(obj_file)[0] + ".glb"
            print(obj_file)
            bpy.ops.object.select_all(action='SELECT')
            bpy.ops.object.delete()
            bpy.ops.import_scene.obj(filepath=obj_file)
            bpy.ops.object.select_all(action='SELECT')
            bpy.ops.export_scene.gltf(filepath=glb_file,export_copyright="© 2021 CIWTA.org")
