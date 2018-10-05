module App

open System
open Canvas
open Fable.Core.JsInterop
open Fable.Import

type Settings =
    { /// Size of a particle
      Size : int
      /// Speed apply to the first particle (the one who follow the mouse)
      FollowSpeed : float
      /// Number of particles to draw
      NumOfSegments : int
      CanvasWidth : float
      CanvasHeight : float }
    static member Default: Settings =
        { Size = 25
          FollowSpeed = 0.1
          NumOfSegments = 16
          CanvasWidth = 600.
          CanvasHeight = 600. }

type Position =
    { X : float
      Y : float }
    static member Empty =
        { X = 0.
          Y = 0. }

[<RequireQualifiedAccess>]
module Particle =

    type Particle =
        { /// X coordinate
          X : float
          /// Y coordinate
          Y : float
          /// Unique Id, it's index based so we can find the previous particle
          Id : int
          /// Force applied, to the X coordinate
          Dx : float
          /// Force applied, to the Y coordiate
          Dy : float }

    let create index : Particle =
        { X = -50.
          Y = -50.
          Dx = 0.
          Dy = 0.
          Id = index + 1 }

    type Settings =
        { Width : float
          Height : float
          FollowSpeed : float
          MousePos : Position }

    let update (particle : Particle) (particles : Particle array) (settings : Settings) delta =
        // If this is not the first particle, then it follows the previous particle
        if particle.Id > 1 then
            // Target we are aiming at
            let aim = particles.[particle.Id - 1 - 1]
            let dx = aim.X - particle.X
            let dy = aim.Y - particle.Y

            { particle with X = particle.X + dx * settings.FollowSpeed * delta
                            Y = particle.Y + dy * settings.FollowSpeed * delta
                            Dx = dx
                            Dy = dy }
        else
            // Follow the mouse
            let dx = settings.MousePos.X - particle.X
            let dy = settings.MousePos.Y - particle.Y

            { particle with X = particle.X + dx * settings.FollowSpeed * delta
                            Y = particle.Y + dy * settings.FollowSpeed * delta
                            Dx = dx
                            Dy = dy }

    let draw ctx (particle : Particle) (total : int) (width : int) =
        Canvas.WithContext(ctx, fun ctx ->
            let width = float width
            let scale = Math.Cos(Math.PI / 2. * (float particle.Id / float total))
            ctx.translate(particle.X, particle.Y)
            ctx.rotate(Math.Atan2(particle.Dy, particle.Dx))
            ctx.scale(scale, scale)
            ctx.fillStyle <- !^"white"
            Canvas.Path(ctx,
                (-width / 2. * 1.732, -width / 2.),
                (0. ,0.),
                (-width / 2. * 1.732, width / 2.),
                (-width / 2. * 1.2, 0.)
            )
        )

module Demo =

    type Model =
        { /// Particles datas
          /// We use array because they are smaller in memory
          Particles : Particle.Particle array
          Settings : Settings
          MousePosition : Position }

    type Msg =
        | MouseMove of Position

    let init (settings: Settings) =
        { Particles = [| for index = 0 to settings.NumOfSegments do
                            yield Particle.create index |]
          Settings = settings
          MousePosition = { X = 0.; Y = 0. }
        }

    let msgUpdate model msgs _timestamp _delta =
        let position =
            (model.MousePosition, msgs) ||> List.fold (fun _pos -> function
                | MouseMove newPosition -> newPosition
            )
        { model with MousePosition = position }

    let timeUpdate model delta =
        let delta = delta / 10.
        // Update all particles positions
        let particles =
            model.Particles
            |> Array.map (fun particle ->
                let settings : Particle.Settings =
                    { Width = model.Settings.CanvasWidth
                      Height = model.Settings.CanvasHeight
                      FollowSpeed = model.Settings.FollowSpeed
                      MousePos = model.MousePosition }
                Particle.update particle model.Particles settings delta
            )
        { model with Particles = particles }

    let view (model : Model) (ctx: Context) _interpolationPercentage =
        ctx.clearRect(0., 0., model.Settings.CanvasWidth, model.Settings.CanvasHeight)
        for particle in model.Particles do
            Particle.draw ctx particle model.Particles.Length model.Settings.Size

    let subscribe (canvas: Browser.HTMLCanvasElement) dispatch (model : Model) =
        canvas.width <- model.Settings.CanvasWidth
        canvas.height <- model.Settings.CanvasHeight
        canvas.addEventListener_mousemove(fun ev ->
            let bounds : Browser.ClientRect = (ev.target :?> Browser.HTMLElement).getBoundingClientRect()
            { X = ev.clientX - bounds.left
              Y = ev.clientY - bounds.top }
            |> MouseMove
            |> dispatch
        )

// App
Canvas.Start("canvas", Demo.init Settings.Default, Demo.msgUpdate, Demo.timeUpdate, Demo.view, Demo.subscribe)
