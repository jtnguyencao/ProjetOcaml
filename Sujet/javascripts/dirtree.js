function dirtree(slide)
  {
    var togglees = document.getElementsByClassName("folder") ;

    for (const togglee of togglees)
     {
       togglee.addEventListener("click", function()
         {
           console.log(this.parentElement.innerHTML) ;
           var parent = this.parentElement.querySelector(".subtree")
           if (parent != null) parent.classList.toggle("subtree-open") ;
           this.classList.toggle("folder-open") ;
         }) ;
     }
  }
