module Canvas

open Fable.Import

type Context = Browser.CanvasRenderingContext2D
type Update<'Msg, 'Model> = 'Model -> 'Msg -> 'Model
type View<'Msg, 'Model> = Context -> ('Msg->unit) -> 'Model -> unit

let private onAnimationFrame tick =
    let mutable last = 0.
    let mutable disposed = false
    let rec loop ts: unit =
        if not disposed then
            if last > 0. then
                tick (ts - last)
            last <- ts
            Browser.window.requestAnimationFrame(loop) |> ignore
    Browser.window.requestAnimationFrame(loop) |> ignore
    { new System.IDisposable with
        member __.Dispose() = disposed <- true }

type Canvas =
    static member Path(ctx: Context, ?fillStyle, ?points: _ seq) =
        match points with
        | None -> () // Throw error?
        | Some points ->
            let mutable init = false
            ctx.beginPath()
            for (x, y) in points do
                if not init then
                    init <- true
                    ctx.moveTo(x, y)
                else
                    ctx.lineTo(x, y)
            fillStyle |> Option.iter (fun x -> ctx.fillStyle <- x)
            ctx.fill()

    static member WithContext(ctx: Context, ?translate, ?rotate, ?scale, ?render: Context->unit) =
        match render with
        | None -> () // Throw error?
        | Some render ->
            ctx.save()
            Option.iter ctx.translate translate
            Option.iter ctx.rotate rotate
            Option.iter ctx.scale scale
            render ctx
            ctx.restore()

    // TODO: This should be done automatically at the beginning of each view
    // But we need a way to check the canvas width and height
    static member Clear(ctx: Context, canvasWidth, canvasHeight) =
        ctx.clearRect(0., 0., canvasWidth, canvasHeight)

    static member Start<'Msg,'Model>(canvasId: string, init: 'Model, tick: float->'Msg,
                                        update: Update<'Msg,'Model>, view: View<'Msg,'Model>,
                                        ?subscribe: Browser.HTMLCanvasElement->('Msg->unit)->unit) =

        let canvasEl = Browser.document.getElementById(canvasId) :?> Browser.HTMLCanvasElement
        let ctx = canvasEl.getContext_2d()
        let mutable model = init
        // TODO: Implement asynchronous commands
        let dispatch msg =
            model <- update model msg
        subscribe |> Option.iter (fun f -> f canvasEl dispatch)
        // TODO: Implement pause/resume mechanism
        let disp = onAnimationFrame <| fun delta ->
            model <- tick delta |> update model
            view ctx dispatch model
        ()
