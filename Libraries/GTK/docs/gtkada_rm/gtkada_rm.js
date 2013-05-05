var current_page='';

function switchPage (name) {
  if (current_page == name) {
     return true;
  }
  current_page = name;

  var children =
     document.getElementById ('documentation').getElementsByTagName ('div');
  for (var i=0; i < children.length; i++) {
     if (children[i].id == 'notebook_' + name) {
        children[i].style.display = 'block';
     } else if (children[i].className == 'notebookPage') {
     children[i].style.display = 'none';
     }
  }

  children = document.getElementById ('notebook').getElementsByTagName ('li');
  for (i=0; i < children.length; i++) {
     if (children[i].id == 'tab_' + name) {
         children[i].className = 'current';
     } else {
         children[i].className = '';
     }
  }

  return true;
}

/************************************************
 ** Toggle the closing or opening of a tree node
 ************************************************/

function setTreeState (li, img, close) {
   if (close) {
      img.src = "treeclose.png";
      img.alt = '[+]';
   } else {
      img.src = "treeopen.png";
      img.alt = '[-]';
   }

   var ul = li.nextSibling;
   while (ul && ul.nodeName != 'UL' && ul.nodeName != 'LI') {
      ul = ul.nextSibling;
   }
   if (ul && ul.nodeName == 'UL') {
      if (close) {
         ul.style.display = 'none';
      } else {
         ul.style.display = 'block';
      }
   }
}

function treetoggle (widget) {
   var img = widget.getElementsByTagName ('img');
   var li  = widget.parentNode;
   var is_expanded = (img[0].src.indexOf ('treeopen') > -1);
   setTreeState (li, img[0], is_expanded);
}


function treeChangeFoldAll(fold) {
   var children = document.getElementById ('widgetTree').getElementsByTagName ('LI');
   var img, m;
   for (var c=0; c < children.length; c++) {
      a = children[c].getElementsByTagName ('a');
      for (m=0; m < a.length; m++) {
         if (a[m].className == 'tree') {
            setTreeState (children[c], a[m].getElementsByTagName ('img')[0], fold);
         }
      }
   }
}

/************************************************
 ** Return the window's height
 ********************************************** */

function getWindowHeight() {
  var wh=0;
  if ( typeof window.innerHeight != 'undefined')
    wh = window.innerHeight;
  else {
    d = document;
   if ( d.documentElement
       && typeof d.documentElement.clientHeight != 'undefined'
       && d.documentElement.clientHeight != 0)
     wh = d.documentElement.clientHeight;
   else if (d.body
            && typeof d.body.clientHeight != 'undefined' )
     wh = d.body.clientHeight;
   else
     alert ("Can't identify window height")
  }
  return wh;
}

/* Adjust the height of various elements to adapt to the screen size */
function adjust_height() {
   /* Do nothing at present (see comment about automatic scrollbars in
      gtkada_rm.css */
   return;

   var screenHeight = getWindowHeight();

   var objectName = document.getElementById ('objectName');
   screenHeight = screenHeight - objectName.clientHeight - 10;

   var rightSide = document.getElementById ('rightSide');
   var h = rightSide.getElementsByTagName ('h2');
   screenHeight = screenHeight - h[0].clientHeight - 20;

   /* The following code works with Opera, but not firefox */
   var ul = rightSide.getElementsByTagName ('ul');
   ul[0].style.maxHeight = screenHeight;
   ul[0].style.height = screenHeight;

   var divs = document.getElementsByTagName ('div');
   for (var n=0; n < divs.length; n++) {
       if (divs[n].className == 'notebookPage') {
          divs[n].style.maxHeight = screenHeight;
          divs[n].style.height    = screenHeight;
       }
   }
}
