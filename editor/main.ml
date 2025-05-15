open Tk
open Infer

let height = 20
let width = 150

let top = openTk ()

let typesLbl = Label.create ~background:(`Color "#251916") ~foreground:(`White) ~justify:`Left ~anchor:`Nw ~height ~width:(width / 2) top

let text = Text.create ~background:(`Color "#302421") ~foreground:(`White) ~height:height ~width:(width / 2) top
let () = Text.tag_configure ~tag:"base_color" ~foreground:(`White) text
let () = Text.tag_configure ~tag:"value" ~foreground:(`Color "#ebacfc") text
let () = Text.tag_configure ~tag:"string" ~foreground:(`Color "#e0f5bf") text
let () = Text.tag_configure ~tag:"symbol" ~foreground:(`Color "#f3ab43") text

let readFile filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let writeFile filename content = 
  let oc = open_out filename in 
  Printf.fprintf oc "%s" content;
  close_out oc

let insertLine i s =
  let open Infer.Parser in 
  let tokenized_line = Infer.Util.tokenize_line s in
  let rec loop = function
  | [] -> ()
  | (s, tok) :: t -> begin
    let tag = match tok with 
      | INT _ | TRUE -> "value"
      | STRING _ -> "string"
      | LET | IN | IF | ELSE | THEN -> "symbol"
      | _ -> "base_color"
    in
    Text.insert ~tags:[tag] ~index:(`Linechar (i, 0), [`Lineend]) ~text:(s ^ " ") text;
    loop t
  end
  in 
  loop tokenized_line

let openFile () = 
  let f = Tk.getOpenFile ~parent:top () in
  if f = "" then () else
  Text.delete ~start:(`Linechar (0, 0), [`Linestart]) ~stop:(`End, [`Linestart]) text;
  let content = readFile f in
  (* Text.insert ~tags:["base_symbol"] ~index:(`Linechar (0, 0), [`Linestart]) ~text:content text *)
  let lines = String.split_on_char '\n' content in
  List.iteri (fun i s -> insertLine i s) lines

let saveFile () =
  let f = Tk.getSaveFile ~parent:top () in
  if f = "" then () else
  writeFile f (Text.get ~start:(`Linechar (0, 0), [`Linestart]) ~stop:(`End, [`Linestart]) text)

let writeTypes _ =
  let codeTxt = Text.get ~start:(`Linechar (0, 0), [`Linestart]) ~stop:(`End, [`Linestart]) text in
  let toWrite = try
    let buf = Buffer.create 256 in
    StringDict.iter (fun x t ->
      let line = Printf.sprintf "%s: %s\n" x (Util.string_of_type t) in
      Buffer.add_string buf line
    ) (infer codeTxt);
    Buffer.contents buf
  with NoUnifier (t1, t2) ->
    Printf.sprintf "No unifier:\n%s <> %s" (Util.string_of_type t1) (Util.string_of_type t2)
  in
  Label.configure ~text:toWrite typesLbl

let buttonOpen = Button.create ~text:"Ouvrir" ~command:openFile top
let buttonSave = Button.create ~text:"Enregistrer" ~command:saveFile top
let buttonQuit = Button.create ~text:"Quitter" ~command:closeTk top

let () =
  bind ~events:[`Modified ([`Control], `KeyPressDetail "p")] ~action:writeTypes ~fields:[] top

let _ = grid [coe text] ~columnspan:3 ~column:1 ~row:1 ~sticky:"nsew"
let _ = grid [coe typesLbl] ~rowspan:2 ~column:4 ~row:1 ~sticky:"nsew"
let _ = grid [coe buttonOpen] ~column:1 ~row:2
let _ = grid [coe buttonSave] ~column:2 ~row:2
let _ = grid [coe buttonQuit] ~column:3 ~row:2

let _ = mainLoop ()


