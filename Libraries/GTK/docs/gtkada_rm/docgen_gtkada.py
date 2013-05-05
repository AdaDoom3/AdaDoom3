"""
   Docgen plugin to generate the GtkAda Reference Manual.

   Invoke with:
      gps -Pgenerate_docs.gpr  --load=python:docgen_gtkada.py

"""

import GPS, re, os
import shutil

class ScreenshotTagHandler (GPS.DocgenTagHandler):
   """Handling for screenshots"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ (
        self, "screenshot",
        on_match = self.on_match,
        on_start=self.on_start,
        on_exit=self.on_exit)

   def on_start (self, docgen):
      self.pictureslist = {}

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      file = docgen.get_current_file()
      srcdir = os.path.normpath (GPS.Project.root().file().directory())
      fullfile = os.path.join (srcdir, value.strip())

      screenshot_found = False
      for extension in ["", ".png", ".jpg"]:
          if os.path.exists(fullfile + extension):
              pict = value.strip() + extension
              screenshot_found = True
              break
      if not screenshot_found:
          GPS.Console ("Messages").write ("could not find screenshot %s\n" % (fullfile))
          return ""

      docdir = os.path.join (docgen.get_doc_dir ().name(), "screenshots")
      if not os.path.exists (docdir):
        os.mkdir(docdir)

      shutil.copy (os.path.join(srcdir,pict), docdir)

      img = """<img src="screenshots/%s" alt="%s" style="border: 0px;"/>""" % (pict, pict)

      self.pictureslist[entity_name] = [entity_href, img]
      return """</div>
        <div class='profile'>
          <h3>Screenshot</h3>
          %s
        </div>
        <div class='comment'>""" % (img)

   def on_exit (self, docgen):
      if len (self.pictureslist) == 0:
         return

      # first print the right-side box containing the group index
      content=""
      content += """<div class='default' id='rightSide'>"""
      content += """<div id='rightSideInside'>"""
      content += """<div id='Index'>"""
      content += """<h2>Index</h2>"""
      content += """<ul>"""
      n = 0
      for pict in sorted(self.pictureslist.keys()):
         content += """<li><a href="#%d">%s</a></li>""" % (n, pict)
         n += 1
      content += """</ul></div></div></div>"""

      content += """<div class='default' id='documentation'>"""
      content += """<div class="title">Widget Screenshots</div>"""
      n = 0
      for pict in sorted(self.pictureslist.keys()):
         content += """
            <div class='subprograms'>
              <div class='class'>
                <a name="%d"></a>
                <h3>%s</h3>
                <div class='comment'>
                  <a href="%s">%s</a>
                </div>
              </div>
            </div>""" % (n, pict, self.pictureslist[pict][0], self.pictureslist[pict][1])
         n += 1
      content += """</div>"""

      docgen.generate_index_file ("Widget Screenshots", "screenshots.html", content);

class ExampleTagHandler (GPS.DocgenTagHandler):
   """Handling for <example> tags"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ (
        self, "example",
        on_match = self.on_match)

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      return """<div class="code">%s</div>""" % value

def on_gps_start (hook):
   # Disable launching of browser on exit
   GPS.Preference ("Doc-Spawn-Browser").set(False)

   GPS.Docgen.register_tag_handler (ScreenshotTagHandler ())
   GPS.Docgen.register_tag_handler (ExampleTagHandler ())
   GPS.Docgen.register_css (GPS.File ("gtkada.css"))
   GPS.Docgen.register_main_index (GPS.File ("gtkada_rm/groups.html"))

   GPS.Project.root().generate_doc (True)
   GPS.Timeout (500, wait_doc)

def wait_doc(timeout):
   txt = GPS.Console("Messages").get_text()
   if txt.find("Documentation generated") == -1:
       return True
   else:
       # Log the output in log.txt
       f=open("log.txt", "w")
       f.write(GPS.Console ("Messages").get_text())
       f.close()
       GPS.exit()

GPS.Hook ("gps_started").add (on_gps_start)
