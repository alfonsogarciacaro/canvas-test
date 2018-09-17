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
          Dy : float
          AngleX : float
          AngleY : float
          SpeedX : float
          SpeedY : float
          Radius : float }

    let private rand = new Random()

    let create index : Particle =
        { X = -50.
          Y = -50.
          Dx = 0.
          Dy = 0.
          Id = index + 1
          AngleX = Math.PI * 2. * rand.NextDouble()
          AngleY = Math.PI * 2. * rand.NextDouble()
          SpeedX = 0.03 * rand.NextDouble() + 0.03
          SpeedY = 0.03 * rand.NextDouble() + 0.03
          Radius = 150. }

    type Settings =
        { Width : float
          Height : float
          FollowSpeed : float
          MousePos : Position }

    let update (particle : Particle) (particles : Particle array) (settings : Settings) =
        // If this is not the first particle, then it follows the previous particle
        if particle.Id > 1 then
            // Target we are aiming at
            let aim = particles.[particle.Id - 1 - 1]
            let dx = aim.X - particle.X
            let dy = aim.Y - particle.Y

            { particle with X = particle.X + dx * settings.FollowSpeed
                            Y = particle.Y + dy * settings.FollowSpeed
                            Dx = dx
                            Dy = dy }
        else
            // If the mouse never moved, then create random mouvements
            if settings.MousePos.X = 0. && settings.MousePos.Y = 0. then
                let dx = settings.Width / 2. + Math.Cos(particle.AngleX) * particle.Radius - particle.X
                let dy = settings.Height / 2. + Math.Sin(particle.AngleY) * particle.Radius - particle.Y

                { particle with X = settings.Width / 2. + Math.Cos(particle.AngleX) * particle.Radius
                                Y = settings.Height / 2. + Math.Sin(particle.AngleY) * particle.Radius
                                AngleX = particle.AngleX + particle.SpeedX
                                AngleY = particle.AngleY + particle.SpeedY
                                Dx = dx
                                Dy = dy }

            else
                // Follow the mouse
                let dx = settings.MousePos.X - particle.X
                let dy = settings.MousePos.Y - particle.Y

                { particle with X = particle.X + dx * settings.FollowSpeed
                                Y = particle.Y + dy * settings.FollowSpeed
                                Dx = dx
                                Dy = dy }

    let draw ctx (particle : Particle) (total : int) (width : int) =
        let width = float width
        let angle = Math.Atan2(particle.Dy, particle.Dx)
        let scale = Math.Cos(Math.PI / 2. * (float particle.Id / float total))
        Canvas.WithContext(ctx,
            translate = (particle.X, particle.Y),
            rotate = angle,
            scale = (scale, scale),
            render = fun ctx ->
                Canvas.Path(ctx,
                    fillStyle = !^"white",
                    points = [
                        (-width / 2. * 1.732, -width / 2.)
                        (0. ,0.)
                        (-width / 2. * 1.732, width / 2.)
                        (-width / 2. * 1.2, 0.)
                    ])
        )

module Demo =

    type Model =
        { /// Particles datas
          /// We use array because they are smaller in memory
          Particles : Particle.Particle array
          Settings : Settings
          MousePosition : Position }

    type Msg =
        /// The tick message is trigger at 60fps, and is responsible for the animation trigger
        | Tick of float
        /// Event triggered when Mouse move over the canvas
        | MouseMove of Position

        | UpdateCanvasSize of float * float
        | UpdateNumOfSegments of int
        | UpdateFollowSpeed of float
        | UpdateSize of int

    let init (settings: Settings) =
        { Particles =
            [|
                for index = 0 to settings.NumOfSegments do
                    yield Particle.create index
            |]
          Settings = settings
          MousePosition =
            { X = 0.
              Y = 0. } }

    let update model = function
        // Update the animation. TODO: Use delta
        | Tick delta ->
            // Update all particles positions
            printfn "Updating..."
            let particles =
                model.Particles
                |> Array.map (fun particle ->
                    let settings : Particle.Settings =
                        { Width = model.Settings.CanvasWidth
                          Height = model.Settings.CanvasHeight
                          FollowSpeed = model.Settings.FollowSpeed
                          MousePos = model.MousePosition }
                    Particle.update particle model.Particles settings
                )

            { model with Particles = particles }

        // Update the mouse position
        | MouseMove newPosition ->
            { model with MousePosition = newPosition }


        | UpdateCanvasSize (width, height) ->
            { model with Settings =
                            { model.Settings with CanvasWidth = width
                                                  CanvasHeight = height } }

        | UpdateNumOfSegments newNum ->
            { model with Particles =
                            [|
                                for index = 0 to newNum do
                                    yield Particle.create index
                            |]
                         Settings =
                            { model.Settings with NumOfSegments = newNum }
                         MousePosition =
                            { X = 0.
                              Y = 0. } }

        | UpdateFollowSpeed newSpeed ->
            { model with Settings =
                            { model.Settings with FollowSpeed = newSpeed } }

        | UpdateSize newSize ->
            { model with Settings =
                            { model.Settings with Size = newSize } }


    let view settings ctx dispatch (model : Model) =
        Canvas.Clear(ctx, model.Settings.CanvasWidth, model.Settings.CanvasHeight)
        for particle in model.Particles do
            Particle.draw ctx particle model.Particles.Length settings.Size

    let subscribe (canvas: Browser.HTMLCanvasElement) dispatch =
        canvas.addEventListener_mousemove(fun ev ->
            let bounds : Browser.ClientRect = (ev.target :?> Browser.HTMLElement).getBoundingClientRect()
            { X = ev.clientX - bounds.left
              Y = ev.clientY - bounds.top }
            |> MouseMove
            |> dispatch
        )

// App
let settings = Settings.Default
Canvas.Start("canvas", Demo.init settings, Demo.Tick,
            Demo.update, Demo.view settings, subscribe = Demo.subscribe)
