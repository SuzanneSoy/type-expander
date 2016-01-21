#lang racket

(require scribble/manual
         (only-in scribble/core make-style)
         (only-in scribble/html-properties alt-tag attributes))

(provide forkongithub)

(define css-code
  #<<EOF
#forkongithub a{
  background:#007200;
  color:#fff;
  text-decoration:none;
  font-family:arial,sans-serif;
  text-align:center;
  font-weight:bold;
  font-size:1rem;
  line-height:2rem;
  position:relative;
  /* transition:0.5s; */
}
#forkongithub a:hover{
  background:#00802B;
  color:#fff;
}
#forkongithub a{
  border-radius: 1ex;
  box-shadow: inset 0px -1px 4px rgba(255,255,255,0.8);
  padding:6.5px 10px 5px 10px;
}
@media screen and (min-width:720px){
  #forkongithub{
    position:absolute;
    display:block;
    top:0;
    right:0;
    width:220px;
    overflow:hidden;
    height:220px;
    z-index:9999;
  }
  #forkongithub a{
    padding:6.5px 40px 5px 40px;
    width:210px;
    position:absolute;
    top:60px;
    right:-67px;
    transform:rotate(45deg);
    -webkit-transform:rotate(45deg);
    -ms-transform:rotate(45deg);
    -moz-transform:rotate(45deg);
    -o-transform:rotate(45deg);
    box-shadow: 0px 5px 4px rgba(0,0,0,0.8);
  }
#forkongithub a::before,
#forkongithub a::after{
  content:"";
  width:100%;
  display:block;
  position:absolute;
  top:2px;
  left:0;
  height:2px;
  background: linear-gradient(90deg, transparent 50%, #bbb 50%)
              repeat scroll 0% 0% / 10px;
}
#forkongithub a::after{
  bottom:2px;
  top:auto;
}
}
EOF
  )

(define (forkongithub href text)
  (elem
   (elem #:style (make-style #f (list (alt-tag "style")))
         css-code)
   (elem #:style (make-style #f (list (alt-tag "span")
                                      (attributes '((id . "forkongithub")))))
         (elem #:style (make-style #f (list (alt-tag "a")
                                            (attributes `((href . ,href)))))
               text))))