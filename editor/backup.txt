open Tk

let top = openTk ()

let text = Text.create ~width:150 ~height:50 top

let readFile filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let writeFile filename content = 
  let oc = open_out filename in 
  Printf.fprintf oc "%s" content;
  close_out oc


let openFile () = 
  let f = Tk.getOpenFile ~parent:top () in
  if f = "" then () else
  Text.delete ~start:(`Atxy (0, 0), [`Linestart]) ~stop:(`End, [`Linestart]) text;
  let content = readFile f in
  Text.insert ~index:(`Atxy (0, 0), [`Linestart]) ~text:content text

let saveFile () =
  let f = Tk.getSaveFile ~parent:top () in
  if f = "" then () else
  writeFile f (Text.get ~start:(`Atxy (0, 0), [`Linestart]) ~stop:(`End, [`Linestart]) text)

let buttonOpen = Button.create ~text:"Ouvrir" ~command:openFile top
let buttonSave = Button.create ~text:"Enregistrer" ~command:saveFile top
let buttonQuit = Button.create ~text:"Quitter" ~command:closeTk top

let _ = grid [coe text] ~columnspan:3 ~column:1 ~row:1
let _ = grid [coe buttonOpen] ~column:1 ~row:2
let _ = grid [coe buttonSave] ~column:2 ~row:2
let _ = grid [coe buttonQuit] ~column:3 ~row:2

let _ = mainLoop ()



