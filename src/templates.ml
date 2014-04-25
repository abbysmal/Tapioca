open Eliom_content
open Html5.D
open Html5.F

let format_page content =
  (Eliom_tools.F.html
     ~title:"Tapioca: An experimental collaborative editor written with Ocsigen"
     ~css:[["css";"tapioca.css"];["css";"bootstrap.css"];["css";"bootstrap-theme.css"]]
     ~js:[["libs.js"]]
     (body [content]))
