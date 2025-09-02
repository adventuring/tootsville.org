declare module 'vue-three' {
  import { Component } from 'vue'
  import { Object3D, Camera, Clock } from 'three'

  export interface CanvasProps {
    camera?: any
    gl?: any
    shadows?: boolean
    onCreated?: (context: any) => void
    onPointerMove?: (event: any) => void
    onClick?: (event: any) => void
  }

  export interface HtmlProps {
    position?: any
    transform?: boolean
    occlude?: boolean
    distanceFactor?: number
    sprite?: boolean
    zIndexRange?: [number, number]
  }

  export interface UseFrameContext {
    clock: Clock
    camera: Camera
    scene: Object3D
    renderer: any
  }

  export const Canvas: Component<CanvasProps>
  export const Html: Component<HtmlProps>
  export function useFrame(callback: (context: UseFrameContext) => void): void
  export function useThree(): {
    scene: Object3D
    camera: Camera
    renderer: any
    clock: Clock
  }
}

