// Learn more about F# at http://fsharp.net


open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.Timers

// Physical Constants

let l0 = 150.
let k = 10.
let u = 0.5
let m = 200.




// RESIZE IMAGE
let resizeImage (path:string, width:int, height:int) = 
    let im = Image.FromFile(path)
    let b = new Bitmap(width, height)
    use g = Graphics.FromImage(b)
    g.DrawImage(im, new RectangleF(0.f, 0.f, float32 width, float32 height))
    im.Dispose()
    b


type Lightweight() = 

  let mutable pos = new PointF(0.f,0.f)
  let mutable area = RectangleF(pos.X,pos.Y,50.f,50.f)
  let mutable text = ""
  let mutable img:Image = null
  let mutable pressed = false

  

  member x.Area
    with get() = area
    and set(a) = area <- a

  member x.Text
    with get() = text
    and set(t) = text <- t

  member x.Img
    with get() = text
    and set(i) = img <- i

  member x.Pos
    with get() = pos
    and set(p) = pos <- p


  member this.IsMouseOver (x, y) =
    area.Contains(x, y)
  
  member this.MouseDown (x, y) =
    pressed <- true

  member this.MouseUp (x, y) =
    pressed <- false

  member this.IsPressed() =
    pressed
    

  member x.paint (g:Graphics, f:Font) =
    if img <> null then g.DrawImage(img, area)
    g.DrawString(text, f, (if pressed then Brushes.Red else Brushes.Black), pos)


// ARC
type Arc() as ar =
  let mutable a = 0
  let mutable b = 0

  member ar.A
    with get() = a
    and set(an) = a <- an

  member ar.B
    with get() = b
    and set(bn) = b <- bn



// N O D E

type Node() as n =
  let mutable pos = new PointF(0.f,0.f)
  let mutable area = RectangleF(pos.X,pos.Y,50.f,50.f)
  let mutable text = ""
  let mutable img:Image = null
  let mutable imgpath = ""
  let mutable pressed = false
  let mutable mouseoffset = new PointF(0.f,0.f)
  let mutable acc = new PointF(0.f, 0.f)
  let mutable vel = new PointF(0.f, 0.f)
  

  member x.Area
    with get() = area
    and set(a) = area <- a

  member x.Text
    with get() = text
    and set(t) = text <- t

  member x.Img
    with get() = img
    and set(i) = img <- i

  member x.Pos
    with get() = pos
    and set(p) = pos <- p

  member x.ImgPath
    with get() = imgpath
    and set(ip) = imgpath <- ip

  member x.Acc
    with get() = acc
    and set(ac) = acc <- ac

  member x.Vel
    with get() = vel
    and set(v) = vel <- v

  member this.IsMouseOver (x, y) =
    area.Contains(x, y)
  
  member this.MouseDown (x, y) =
    pressed <- true
    mouseoffset.X <- x - pos.X
    mouseoffset.Y <- y - pos.Y

  member this.IsPressed() =
    pressed

  member this.MouseUp (x, y) =
    pressed <-false
    
  member this.OnMouseMove(x, y) =
       if pressed then 
        n.Pos <- new PointF(x - mouseoffset.X,y - mouseoffset.Y)
        this.Area <- new RectangleF(pos.X,pos.Y,area.Width,area.Height)

  member this.update() =
    pos <- new PointF(pos.X + vel.X, pos.Y + vel.Y)
    area <- new RectangleF(pos.X,pos.Y,area.Width, area.Height)
                       

  member x.paint (g:Graphics, f:Font) =
    if img <> null then g.DrawImage(img, area)
    g.DrawString(text, f, (if pressed then Brushes.Red else Brushes.Black), new PointF(pos.X + area.Width/2.f, pos.Y+area.Height/2.f))
    let s = n.Pos.ToString()
    if pressed then g.DrawString(s,f, Brushes.Brown,new PointF(0.f,0.f))
  



  // PHYSICS SIMULAION FUNCTION
let simSpring (nodes:ResizeArray<Node>, arcs:ResizeArray<Arc>, dtime) =
    for n in nodes do
        let vvectors = new ResizeArray<PointF>()
        for a in arcs do
            if a.A=nodes.IndexOf(n) then
                let othern = nodes.Item(a.B)
                let l = Math.Sqrt( Math.Pow(float(n.Pos.X - othern.Pos.X),2.) + Math.Pow(float(n.Pos.Y - othern.Pos.Y),2.) )
                let f = -k*(l-l0)*u
                let a = f/m
                let v = new PointF( (n.Pos.X - othern.Pos.X)/float32(l)* float32 (a*dtime),(n.Pos.Y - othern.Pos.Y)/float32(l) * float32(a*dtime))
                vvectors.Add(v)
        for a in arcs do
            if a.B=nodes.IndexOf(n) then
                let othern = nodes.Item(a.A)
                let l = Math.Sqrt( Math.Pow(float(n.Pos.X - othern.Pos.X),2.) + Math.Pow(float(n.Pos.Y - othern.Pos.Y),2.) )
                let f = -k*(l-l0)*u
                let a = f/m
                let v = new PointF( (n.Pos.X - othern.Pos.X)/float32(l)* float32 (a*dtime),(n.Pos.Y - othern.Pos.Y)/float32(l) * float32(a*dtime))
                vvectors.Add(v)
        let mutable newvel = new PointF(0.f, 0.f)
        for v in vvectors do
            newvel <-new PointF(newvel.X + v.X, newvel.Y + v.Y)
        n.Acc <- new PointF(newvel.X - n.Vel.X/float32(dtime) ,  newvel.Y - n.Vel.Y/float32(dtime))
        n.Vel <- new PointF(newvel.X + n.Vel.X, newvel.Y + n.Vel.Y)
        




    
// CONTAINER
//é un controllo quindi non deve resizarsi con la form!
type Container() as x =
    inherit UserControl()

    let childcontrols = new ResizeArray<Node>()
    let buttons = new ResizeArray<Lightweight>()
    let arcs = new ResizeArray<Arc>()

    let mutable imgpath = "C:\Users\Giulio\Desktop\climtea.jpg"
    let mutable lastmousepos = new PointF(0.f,0.f)
    let r = new System.Random()
    let mutable selectednode = -1
    let timer = new System.Windows.Forms.Timer(Interval=10000)
    let simtimer = new System.Windows.Forms.Timer(Interval = 17)
    let mutable doubleclicked = false

    let mutable viewpos = (0.f, 0.f)
    let mutable zoom = (1.f, 1.f)

    let imagebutton = new Lightweight(Text = "Image", Pos = new PointF(100.f,10.f), Area = new RectangleF(100.f,15.f,30.f,20.f))
    let clearbutton = new Lightweight(Text = "Clear", Pos = new PointF(200.f,10.f), Area = new RectangleF(200.f,15.f,30.f,20.f))
    let leftscroll = new Lightweight(Text="<",Pos = new PointF(325.f,10.f), Area=new RectangleF(325.f, 5.f, 25.f, 25.f))
    let rightscroll = new Lightweight(Text=">",Pos = new PointF(350.f,10.f), Area=new RectangleF(350.f, 5.f, 25.f, 25.f))
    let topscroll = new Lightweight(Text="^",Pos = new PointF(375.f,10.f), Area=new RectangleF(375.f, 5.f, 25.f, 25.f))
    let bottomscroll = new Lightweight(Text="V",Pos = new PointF(400.f,10.f), Area=new RectangleF(400.f, 5.f, 25.f, 25.f))
    let pluszoom = new Lightweight(Text="+",Pos = new PointF(425.f,10.f), Area=new RectangleF(425.f, 5.f, 25.f, 25.f))
    let lesszoom = new Lightweight(Text="-",Pos = new PointF(450.f,10.f), Area=new RectangleF(450.f, 5.f, 25.f, 25.f))
    let sim = new Lightweight(Text="Start Sim",Pos = new PointF(475.f,10.f), Area=new RectangleF(475.f, 5.f, 25.f, 25.f))
    

    do
        x.SetStyle(ControlStyles.OptimizedDoubleBuffer ||| ControlStyles.AllPaintingInWmPaint,true)
        buttons.Add(imagebutton)
        buttons.Add(clearbutton)
        buttons.Add(leftscroll)
        buttons.Add(rightscroll)
        buttons.Add(topscroll)
        buttons.Add(bottomscroll)
        buttons.Add(pluszoom)
        buttons.Add(lesszoom)
        buttons.Add(sim)
        timer.Tick.Add( fun _ ->
            timer.Stop()
            doubleclicked <- false
            x.Invalidate() )

        simtimer.Tick.Add( fun _ ->
            simSpring(childcontrols, arcs,0.017)
            for n in childcontrols do
                n.update()
            x.Invalidate()
            )

        

    // MEMBERS
    member x.Add(lw:Node) =
        childcontrols.Add(lw)

    member x.Remove(lw:Node) =
        childcontrols.Remove(lw)


    // MOUSE UP
    override this.OnMouseUp e =
        let mutable x,y = float32(e.X), float32(e.Y)
        
        if buttons.Item(0).IsPressed() && selectednode <> -1 && buttons.Item(0).IsMouseOver(x,y) then
             use fd = new OpenFileDialog()
             if fd.ShowDialog() = DialogResult.OK then
                    let n = childcontrols.Item(selectednode) 
                    n.Img <- resizeImage(fd.FileName, 50,50)
                    n.ImgPath <- fd.FileName
                    
                    
        elif buttons.Item(1).IsPressed()  && buttons.Item(1).IsMouseOver(x,y) then
            childcontrols.Clear()
            arcs.Clear()
            selectednode <- -1
            doubleclicked <- false

        elif buttons.Item(8).IsPressed() && buttons.Item(8).IsMouseOver(x,y) then
            if  buttons.Item(8).Text ="Start Sim" then 
                simtimer.Start()
                buttons.Item(8).Text <- "Stop Sim"
            else
                simtimer.Stop()
                buttons.Item(8).Text <-"Start Sim"

        for b in buttons do
               b.MouseUp(x,y)

        
        x <- (x / (fst zoom) - (fst viewpos))
        y <- (y / (snd zoom) - (snd viewpos))

        for c in childcontrols do
            c.MouseUp(x,y)

        


        this.Invalidate()

    // MOUSE DOWN
    override this.OnMouseDown e =
        let mutable x,y = float32(e.X), float32(e.Y)
        let mutable pressed = false

        for b in buttons do
            if b.IsMouseOver(x,y) && pressed = false then
               b.MouseDown(x,y)
               pressed <- true 
        
        let dist = 25.f * (1.f/fst zoom)
        if buttons.Item(2).IsPressed() then
            let px, py = viewpos
            viewpos <- (px +  dist, py)
        
        if buttons.Item(3).IsPressed() then
            let px, py = viewpos
            viewpos <- (px -  dist, py)

        if buttons.Item(4).IsPressed() then
            let px, py = viewpos
            viewpos <- (px , py +  dist)

        if buttons.Item(5).IsPressed() then
            let px, py = viewpos
            viewpos <- (px, py  -  dist)

        if buttons.Item(6).IsPressed() then
           let zx,zy = zoom
           if zx <5.f && zy < 5.f then zoom <- (zx + 0.2f, zy+ 0.2f)

        if buttons.Item(7).IsPressed() then
           let zx,zy = zoom
           if zx >0.4f && zy > 0.4f then zoom <- (zx - 0.2f, zy- 0.2f)

        x <- (x / (fst zoom) - (fst viewpos))
        y <- (y / (snd zoom) - (snd viewpos))

        for i=0 to childcontrols.Count-1 do
            let c = childcontrols.Item(i)
            if c.IsMouseOver(x,y) && pressed = false then     
                c.MouseDown(x,y)
                pressed <- true
                selectednode <- childcontrols.IndexOf(c)
        
        if pressed = false then 
                let a1 = new Node(Text = "Node "+ childcontrols.Count.ToString(), Img = resizeImage(imgpath,50,50), ImgPath = imgpath,Pos = new PointF(x-25.f,y-25.f),Area = new RectangleF(x-25.f,y-25.f,50.f,50.f))
                a1.MouseDown(x,y)
                pressed <- true
                this.Add(a1)
                selectednode <- childcontrols.Count-1
                match childcontrols.Count with
                    | 0 | 1  -> ()
                    | 2 -> arcs.Add(new Arc(A =childcontrols.Count-1, B = 0))
                    | 3 | 4 | 5 | 6 | 7 | 9 -> 
                        for i=1 to r.Next(1, childcontrols.Count-2) do
                            let mutable alreadyexist = false
                            let target = r.Next(1, childcontrols.Count-2)
                            for a in arcs do
                                if a.A = childcontrols.Count-1 && a.B = target then alreadyexist <- true
                            if alreadyexist = false then   arcs.Add(new Arc(A =childcontrols.Count-1, B = target))
                    | _ -> 
                        for i=1 to r.Next(1, 8) do
                            let mutable alreadyexist = false
                            let target = r.Next(1, childcontrols.Count-2)
                            for a in arcs do
                                if a.A = childcontrols.Count-1 && a.B = target then alreadyexist <- true
                            if alreadyexist = false then   arcs.Add(new Arc(A =childcontrols.Count-1, B = target))
        
            

        this.Invalidate()
            
    // MOUSE MOVE
    override this.OnMouseMove e =
        let x, y = (float32(e.X)/ (fst zoom) - (fst viewpos)), (float32(e.Y)/ (snd zoom) - (snd viewpos))
        for c in childcontrols do
            if c.IsPressed() then 
                        c.OnMouseMove(x,y) 
                        this.Invalidate()
        lastmousepos.X <- x
        lastmousepos.Y <- y
    



     // DOUBLE CLICK
    override this.OnMouseDoubleClick e =
       let x, y = (float32(e.X)/ (fst zoom) - (fst viewpos)), (float32(e.Y)/ (snd zoom) - (snd viewpos))
       for c in childcontrols do
            if c.IsMouseOver(x,y) && selectednode<> -1 then
                doubleclicked <- true
                timer.Start()
                this.Invalidate()
                



                                       
    // PAINT!
    override this.OnPaint e =
    
        e.Graphics.SmoothingMode <- SmoothingMode.AntiAlias
        
        e.Graphics.ScaleTransform(fst zoom, snd zoom)
        e.Graphics.TranslateTransform(fst viewpos, snd viewpos)
        

        if doubleclicked = false then
            for i=0 to arcs.Count-1 do
                let a = arcs.Item(i)
            
                e.Graphics.DrawLine(new Pen(Color.Black),childcontrols.Item(a.A).Pos.X + 25.f, childcontrols.Item(a.A).Pos.Y + 25.f,
                 childcontrols.Item(a.B).Pos.X + 25.f ,childcontrols.Item(a.B).Pos.Y + 25.f)
                done   

            if selectednode <> -1 then
                use p = new Pen(Color.Yellow, 4.f)
                p.DashStyle <- Drawing2D.DashStyle.Dot
                let area = childcontrols.Item(selectednode).Area
                e.Graphics.DrawRectangle(p, Rectangle.Round(area))

            for c in childcontrols do
                    c.paint(e.Graphics,x.Font)
                    e.Graphics.DrawString(lastmousepos.ToString(),x.Font,Brushes.Red, new PointF(0.f, 10.f))
        
          


        e.Graphics.ResetTransform()  
        
        if selectednode <> -1 && doubleclicked then
            e.Graphics.DrawImage(resizeImage(childcontrols.Item(selectednode).ImgPath,this.Width,this.Height), new RectangleF(0.f,0.f,float32 this.Width,float32 this.Height))
                      
        for b in buttons do
            b.paint(e.Graphics, x.Font)
            









let f = new Form(Width = 500, Height = 500)

let con = new Container(Dock=DockStyle.Fill)

f.Controls.Add(con)
f.Show()


