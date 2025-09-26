declare module 'vue-three' {
  import { Component } from 'vue'
  import { Vector3, Euler, Camera, Scene, Renderer, Clock } from 'three'

  export interface FrameData {
    clock: Clock
    scene: Scene
    camera: Camera
    gl: Renderer
  }

  export interface UseFrameCallback {
    (data: FrameData): void
  }

  export function useFrame(callback: UseFrameCallback): void

  export const Html: Component<any, any, any>
  export const Canvas: Component<any, any, any>
}