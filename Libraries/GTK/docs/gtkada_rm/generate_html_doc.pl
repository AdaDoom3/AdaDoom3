#! /usr/bin/env perl

use warnings;
use strict;

## This script generates the GtkAda documentation from the sources
## All the .ads in the src/ subdirectory are searched for relevant
## tags, and the documentation is automatically generated.
## The tags are looked for anywhere in the file.
##
## SETUP NOTES:
##    This script should be run from the $prefix/doc/gtkada_ug
##    directory in the GtkAda package.
##
## The following tags are known:
##
## <description>...</description>
##     This is the description for a package. It can span multiple
##     lines, and contain any kind of text
##     It no such tag is found, no documentation is generated for
##     the package.
##     Multiple such tags are concatenated.
##     Lists are automatically replaced by @itemize items (a list
##     is a set of paragraphs that start with '-'; the list ends
##     at the first empty-line not followed by a paragraph that
##     starts with '-').
##
## <c_version>...</c_version>
##     Version number of gtk+ that was used to generate this file.
##     ex/ 1.2.6
##     The package should be fully synchronized with the c header
##     when this tag is used.
##     If not such tag is found, the package is assumed to be more
##     or less synchronized with gtk+1.2. Some functions might not
##     have an Ada binding.
##
## <screenshot>...</screenshot>
##     Specifies the name of a file that shows a screenshot of the
##     widget. The name should not include any extension (it will
##     be ".eps" for the latex version of the documentation and
##     ".jpg" for the html version.
##     This tag won't be shown in the info file.
##     The pixmap should be in the directory where you are compiling
##     the documentation.
##
## <example>...</example>
##     Gives an example of use of the widget. The keywords and comments
##     are automatically highlighted. Unless the example is short, we
##     recommended using <include> to include its code from an external
##     compilable source file.
##
## <include>...</include>
##     Includes an external source file. The file is included inline, thus
##     any enclosing tag is taken into account, as in
##      <example><include>file</include></example>
##     Note that comment starts are added at the beginning of each line.
##     You can use relative pathnames (relative to the directory of the file
##     containing this include).
##     This is only supported within <example>
##
## <signals>
##    - "signal_name"
##      signal description
##    - "signal_name" ....
## </signals>
##     This tag describes all the new signals defined by a widget.
##
## <properties>
##   -- Name: name
##   -- Type: type
##   -- Descr: descr
## </properties>
##     Describes the properties of the widget
##
## <child_properties>
##     Same syntax as <properties>, documents properties for children
##
## <style_properties>
##     Same syntax as <properties>, documents style properties 
##
## <testgtk>file.adb</testgtk>
##     Specifies the name of the source file in testgtk that contains example
##     for the widget
##
## <group>...</group>
##     Indicates the group in which the package should be listed in the
##     general index.
##
## <see>...</see>
##     The name of a package that the user should also visit for more information
##     on this widget. You can have several such links if there are more than one
##     package.
##
## <doc_ignore>...</doc_ignore>
##     Indicates a section which should not be included in the documentation.
##     All the subprograms inside the section will not be documented. The lines
##     on which these tags are are completly deleted.
##
## The functions and procedures are automatically associated with
## the comments that follow them, and that stops at the first blank
## line.
## No documentation is generated for the following special subprograms:
##   Generate
##

# The list of files to parse
our @source_files = @ARGV;

# The special keywords to highlight in profiles and examples
our @Ada_keywords = ('abort', 'abs', 'accept', 'access', 'all', 'and',
	             'array', 'at', 'begin', 'body', 'case', 'constant',
		     'declare', 'delay', 'delta', 'digits', 'do', 'else',
	             'elsif', 'end', 'entry', 'exception', 'exit', 'for',
		     'function', 'generic', 'goto', 'if', 'in', 'is',
	             'limited', 'loop', 'mod', 'new', 'not', 'null', 'of',
		     'or', 'others', 'out', 'package', 'pragma', 'private',
		     'procedure', 'raise', 'range', 'record', 'rem',
		     'renames', 'return', 'reverse', 'select', 'separate',
	             'subtype', 'task', 'terminate', 'then', 'type',
	             'until', 'use', 'when', 'while', 'with', 'xor');
our @Ada95_keywords = ('abstract', 'aliased', 'protected', 'requeue',
                       'tagged');
our $keywords = join ("|", @Ada95_keywords, @Ada_keywords);

## List of special sections in the Ada files
our @xml_sections = ("description", "example", "screenshot", "c_version",
                     "see", "signals", "properties", "child_properties",
                     "style_properties", "testgtk", "group");

## Will contain, for each widget, the name of its parent type. The parent
## doesn't contain a package name.
our %parents;

## Will contain the name of packages for each widget. Indexed on widget name
## The second variable contains the name of html file for each widget.
our %packages;
our %files_from_widget;
our %files_from_package;

## List of all known entities. Key is the entity name, prefixed by package,
## contents is
##   [$file, $a_name]   ("a_name" is the name of the HTML anchor
our (%entities);

## Will contain info about each of the source files (subprograms,...). Indexed
## on file name.
our %files;

## The groups for the index. Key is the group name, contents is an array of
## package
our %groups;

## The list of types that implements an interface. Index is the interface name (short),
## value is the list of types that implement it
our %implemented;

## The list of screenshots. Index is package name, value is image name
our %screenshots;

#####################
## Name of the HTML output file (suitable for links) for an Ada file
#####################

sub html_from_ada() {
   my ($file) = shift;
   $file =~ s/^.*\/([^\/]+).ads/$1.html/;
   return $file;
}

#####################
## Parse an Ada file, and extra each of the relevant special sections
#####################

our $subprogram_re =
   '\n[ \t]*(?:(?:procedure|function)\s+(\w+|".")\s*(?:\([^)]+\))?[^;]*?;)';
our $empty_comment_re = '(?:[ \t]+--\n)';
our $non_empty_comment_re = '(?:[ \t]+--[^\n]+\n)';
our $non_empty_comment_block_re = '(' . $non_empty_comment_re . '*)';
our $comment_block_re = '((?:[ \t]*--[^\n]*\n)*)';
our $section_and_comment_re =
   "\n[ \t]+--[ \t]+([^\n]+?)[ \t]+--\n[ \t]+--+\n" . $non_empty_comment_block_re;
our $subprogram_and_comment_re =
   "((?:$subprogram_re)+)\n" . $comment_block_re;
our $widget_re =
   '\n\s*type\s+(\w+)\s+is\s+(?:abstract\s+)?(?:tagged\s+private|new\s+([\w\.]+)\s+with)';
our $type_re = 
   '\s*(type\s+(\w+)\sis\s+((?:access\s*(?:function|procedure)\s*\([^)]+\))?[^;]*);)\n' . $comment_block_re;
our $interface_re =
   'is\s+new\s+(?:Glib\.Types\.)?Implements\s*\(([\w\.]+),\s*([\w\.]+)';

sub extract_sections() {
   my ($file) = shift;
   my ($html_file);
   my ($tags) = join ("|", @xml_sections);
   my (%tags, @subprograms, $package, @widgets);
   my ($section, $section_pushed, @sections, %types, @interfaces);
   my ($count) = 1;

   open (FILE, $file) || die "File not found: $file";
   my ($contents) = join ("", <FILE>);
   close (FILE);

   # Find package name
   ($package) = ($contents =~ /^package ([\w\.]+) is/m);
    return if (!defined $package);

   $html_file = &html_from_ada($file);

   # Remove sections that must be ignored
   $contents =~ s/<doc_ignore>.*?<\/doc_ignore>/\n/gs;

   # Find all special tags
   while ($contents =~ /<($tags)>(.*?)<\/\1>/osg) {
      my ($tag, $value) = ($1, $2);
      if (defined $tags{$tag}) {
        $tags{$tag} .= "\n$value";
      } else {
        $tags{$tag} = $value;
      }
   }

   # No <description> ? => No automatic doc should be generated
   if (!defined $tags{'description'}) {
      return;
   }

   $files_from_package{$package} = $html_file;
   $entities{$package} = [$html_file, ""];

   # Store for the index
   if (defined $tags{'group'}) {
      push (@{$groups{$tags{'group'}}}, $package);
   } else {
      push (@{$groups{"Miscellaneous"}}, $package);
   }

   # Store the screenshot
   if (defined $tags{'screenshot'}) {
     my ($screenshot) = $tags{'screenshot'};
     $screenshot .= ".jpg" if (-f "$screenshot.jpg");
     $screenshot .= ".png" if (-f "$screenshot.png");
     $screenshots{$package} = $screenshot;
     $tags{'screenshot'} = $screenshot;
   }

   # Remove private part, after finding special tags. Ignore private part
   # for local packages
   $contents =~ s/\nprivate\s.*//s;

   # Remove these special tags so that they get ignored when looking for
   # subprograms (think of examples and/or signal descriptions). In fact, this
   # might not be needed since these other subprograms will be inside comments
   # and will therefore not match $subprogram_re.
   #$contents =~ s/<($tags)>.*?<\/\1>//osg;

   # Find widget names
   while ($contents =~ /$widget_re/og) {
       my ($widget, $parent) = ($1, $2);
       if (defined $parent) {
          $parent =~ s/^.*?\.(\w+)$/$1/;
          $parents{$widget} = $parent;
       } else {
          $parents{$widget} = "";
       }
       push (@widgets, $widget);
       $packages{$widget} = $package;
       $entities{"$package.$widget"} = [$html_file, ""];
       $files_from_widget{$widget} = $html_file;
   }

   # Find types
   while ($contents =~ /$type_re/og) {
       my ($definition, $name, $parent, $comment) = ($1, $2, $3, $4);
       $entities{"$package.$name"} = [$html_file, ""];
       if ($name eq "GType_Interface") {
          $parents{$name} = "";
          $files_from_widget{$name} = $html_file;
       } elsif ($parent =~ /GType_Interface/) {
          $parents{$name} = "GType_Interface";
          $files_from_widget{$name} = $html_file;
       }
       $types{$name} = [$definition, $comment];
   }

   # Find interfaces
   while ($contents =~ /$interface_re/og) {
      my ($interface, $object) = ($1, $2);
      push (@interfaces, $interface);
      $interface =~ s/^.*?\.([^.]+)$/$1/;
      push (@{$implemented{$interface}}, $object);
   }

   # Find subprograms
   $section = "General";
   push (@sections, [$section, ""]);
   $section_pushed = 0;

   while ($contents =~ /(?:$subprogram_and_comment_re)|(?:$section_and_comment_re)/og) {
      if (defined $1) {
         my ($description, $comment) =  ($1, $3);
         push (@subprograms, [$section, $description, $comment]);
         $section_pushed = 0;

         ## Store the subprograms in the list of entities
         while ($description =~ /($subprogram_re)/og) {
            my ($sname) = ($2);
            $entities {"$package.$sname"} = [$html_file, "${sname}_${count}_"];
            $count ++;
         }

      } else {
         pop (@sections) if ($section_pushed);  ## No contents => ignore
         $section = $4;
         push (@sections, [$section, $5]);
         $section_pushed = 1;
      }
   }
   pop (@sections) if ($section_pushed);  ## No contents => ignore

   $files{$file} = [$package, \%tags, \@subprograms, \@widgets,
                    \@sections, \%types, \@interfaces];
}

#####################
## Return a processed version of the comment
#####################

sub process_comment() {
   my ($comment) = shift;
   my ($package) = shift;
   my ($params)  = shift;
   my (%params)  = ();
   %params  = %{$params} if (defined $params);
   $comment =~ s/^\s*--(  )?//gm;

   # Empty line => Force a new paragraph
   $comment =~ s/\n\n/\n<p>\n/g;

   # Highlight URLs
   $comment =~ s,(http://\S+),<a href="$1">$1<\/a>,g;

   # Highlight internal cross-refs. This is done by detecting words starting
   # with an upper case that reference a known package or widget type
   $comment =~ s/([A-Z](?:\.?\w+)*)/
                  my ($name) = $1;
                  my ($file, $anchor);
                  if (defined $params{$name}) {
                     "<tt>$name<\/tt>";
                  } else {
                    if (defined $entities{$name}) {
                       ($file, $anchor) = @{$entities{$name}};
                    } elsif (defined $entities{"${name}_Record"}) {
                       ($file, $anchor) = @{$entities{"${name}_Record"}};
                    } elsif (defined $entities{"$package.${name}_Record"}) {
                       ($file, $anchor) = @{$entities{"$package.${name}_Record"}};
                    } elsif (defined $entities{"$package.$name"}) {
                       ($file, $anchor) = @{$entities{"$package.$name"}};
                    }

                    if (defined $file) {
                       if ($anchor ne "") {
                          "<a href='$file#$anchor'>$name<\/a>";
                       } else {
                        "<a href='$file'>$name<\/a>";
                       }
                    } else {
                       $name;
                    }
                 }
               /xeg;

   ## Highlight cross-refs to specific file names (only starting with g
   ## so that we do not add xref to files in testgtk, since no html is
   ## generated for these)
   $comment =~ s/\b(g[\w.-]+)(\.ad[bs])/<a href='$1.html'>$1$2<\/a>/g;

   return $comment;
}

#####################
## Display the profile of a subprogram, including xref
#####################

sub process_profile() {
   my ($profile) = shift;

   # Unindent as much as possible
   $profile =~ s/^[ \t]*\n//mg;
   my ($spaces) = ($profile =~ /^(\s*)/);
   $profile =~ s/^$spaces//gm;

   # Remove empty lines
   $profile =~ s/\s*$//;

   # Create xref for types
   $profile =~ s/(:\s*(?:access|in|out)?\s*)([\w.]+)((?:'Class)?(\s*:=\s*\w+)?[;)])/
                 if (defined $entities{$2}) {
                    "$1<a href='$entities{$2}->[0]'>$2<\/a>$3";
                 } else {
                    "$1$2$3";
                 } 
                /xeg;
   $profile =~ s/(return\s+|is\s+new\s+)([\w.]+)/
                 if (defined $entities{$2}) {
                    "$1<a href='$entities{$2}->[0]'>$2<\/a>";
                 } else {
                    "$1$2";
                 } 
                /xeg;

   return &highlight_syntax ($profile);
}

sub highlight_syntax() {
   my ($profile) = shift;

   # Highlight comments
   $profile =~ s/^([ \t]*--.*)/<i>$1<\/i>/mg;

   # Highlight subprogram name (for subprograms section, not examples)
   $profile =~ s/^(procedure|function)\s+(\w+|".")/$1 <span class='name'>$2<\/span>/gi;

   # Highlight keywords, not in comments
   $profile =~ s/\b($keywords)\b/<b>$1<\/b>/og;
   while (($profile =~ s/<i>(.*)<b>(\w+)<\/b>/<i>$1$2/g)){};
   return $profile;
}

######################
## Parse the signals section
######################

our $non_empty_non_signal_comment_re = '(?:[ \t]+--  [^-][^\n]*\n)';
our $non_empty_comment_non_signal_block_re =
   '(' . $non_empty_non_signal_comment_re . '*)';
our $signal_re = '--[ \t]+-[ \t]*"(\w+)"\n'  # Signal name
   . '[ \t]+--[ \t]+((?:procedure|function) Handler[\s-]+\([^)]+\)[\s-]*'
   . '(?:return [\w.]+)?;)\n'
   . $empty_comment_re . '?' # Optional blank line between profile and comment
   . $non_empty_comment_non_signal_block_re;  # comment
                
sub parse_signals() {
   my ($section) = shift;
   my (%signals);

   while ($section =~ /$signal_re/goi) {
      my ($name, $profile, $comment) = ($1, $2, $3);
      $profile =~ s/^\s+--//mg if (defined $profile);
      $signals{$name} = [$profile, $comment];
   }

   return %signals;
}

######################
## Parse the properties section
######################

our $properties_re = '--[ \t]+(?:- )?Name:[ \t]*(.+)\n'
   . '[ \t]+--[ \t]+(?:- )?Type:[ \t]*(.+)\n'
   . '(?:[ \t]+--[ \t]+(?:- )?Flags:[ \t]*(.+\n))?'
   . '(?:[ \t]+--[ \t]+(?:- )?Descr:[ \t]*(.+\n(?:--[ \t]{4,})*))?'
   . '(?:[ \t]+--[ \t]+(?:- )?See also:[ \t]*(.+)\n)?';

sub parse_properties() {
   my ($section) = shift;
   my (%properties);

   while ($section =~ /$properties_re/goi) {
      my ($name, $type, $descr, $see) = ($1, $2, $4, $5);
      $properties{$name} = [$type, $descr, $see];
   }
   return %properties;
}

######################
## Generate a HTML header in the given FILE
######################

sub generate_header() {
   my ($title) = shift;
   local (*FILE) = shift;

   # Headers
   print FILE "<?xml version='1.0' encoding='utf-8' />\n";
   print FILE '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" ',
                ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">', "\n";
   print FILE "<html><head>\n";
   print FILE " <title>GtkAda: $title</title>\n";
   print FILE " <link rel='stylesheet' href='gtkada_rm.css' type='text/css'>\n";
   print FILE " <script src='gtkada_rm.js' type='text/javascript'></script>\n";
   print FILE "</head><body>\n";

   # Page title
   print OUTPUT "<div id='objectName'>\n";
   print OUTPUT " <span><a href='index.html'><img src='home.png' alt='Toc'",
                " title='Table of Contents'/></a>";
   print OUTPUT " <a href='gallery.html'><img src='gallery.png' alt='Gallery'",
                " title='Widgets gallery'/></a>\n";
   print OUTPUT " <a href='entities.html'><img src='entities.png' alt='Index'",
                " title='Global Index'/></a>\n";
   print OUTPUT " <a href='tree.html'><img src='tree.png' alt='Tree'",
                " title='Widgets Inheritance Tree'/></a>\n";
   print OUTPUT "  </span>\n$title\n";
   print OUTPUT "</div> <!--  objectName -->\n\n";
}

######################
## Generate HTML for a package
######################

sub generate_html() {
   my ($filename) = shift;
   my ($output)   = &html_from_ada ($filename);
   my ($package)  = $files{$filename}[0];
   my (%tags)     = %{$files{$filename}[1]};
   my (@subprograms) = @{$files{$filename}[2]};
   my (@widgets)  = @{$files{$filename}[3]};
   my (@sections) = @{$files{$filename}[4]};
   my (%types)    = %{$files{$filename}[5]};
   my (@interfaces) = @{$files{$filename}[6]};
   my ($w, $current);
   my ($parent_package, $parent_file);
   my ($count) = 1;
   my ($has_types) = scalar (keys %types) > 0;

   # Start generating the output

   $w = $widgets[0];
   if (defined $w && defined $parents{$w}) {
      $parent_package = $packages{$parents{$w}};
      $parent_file    = $files_from_widget{$parents{$w}};
   }

   open (OUTPUT, ">gtkada_rm/$output");
   &generate_header ("$package", *OUTPUT);

   ## Left side

   print OUTPUT "<div id='leftSide'>\n";

   ## Screenshot
   if (defined $tags{"screenshot"}) {
      my ($screenshot) = $tags{"screenshot"};
      print OUTPUT "  <div id='screenshot'>\n";
      print OUTPUT "    <h2>Screenshot</h2>\n";
      print OUTPUT "    <img src='$screenshot' alt='No screeshot'/>\n";
      print OUTPUT "  </div>\n\n";
   }

   ## Class hierarchy
   if (defined $widgets[0]) {
      print OUTPUT "  <div id='classHierarchy'>\n";
      print OUTPUT "   <h2>Hierarchy</h2>\n";
      &generate_tree_for_widgets (\@widgets, 0, *OUTPUT);
      print OUTPUT "  </div> <!--  classHierarchy -->\n\n";
   }

   ## Interfaces
   if ($#interfaces >= 0) {
      print OUTPUT "  <div id='interfaces'>\n";
      print OUTPUT "   <h2>Interfaces</h2>\n";
      print OUTPUT "   <ul>\n";
      foreach (@interfaces) {
         my ($name) = $_;
         my ($short) = $name;
         $short =~ s/^.*?\.([^.]+)$/$1/;
         my ($f) = $files_from_widget{$short};
         if (defined $f) {
            print OUTPUT "  <li><a href='$f'>$name</a></li>\n";
         }
      }
      print OUTPUT "   </ul>\n";
      print OUTPUT "  </div> <!-- interfaces -->\n\n";
   }

   ## Implemented by
   my ($has_interface) = 0;
   foreach (keys %types) {
     my ($def) = $types{$_}->[0];
     if ($def =~ /GType_Interface/ && $_ ne "GType_Interface") {
        $has_interface = $_;
     }
   }

   if ($has_interface) {
      print OUTPUT "  <div id='interfaces'>\n";
      print OUTPUT "    <h2>Implemented by</h2>\n";
      print OUTPUT "    <ul>\n";
      foreach (@{$implemented{$has_interface}}) {
         print OUTPUT "  <li><a href='$files_from_widget{$_}'>$_</a></li>\n";
      }
      print OUTPUT "    </ul>\n";
      print OUTPUT "  </div> <!-- interfaces -->\n\n";
   }

   ## Navigation
   print OUTPUT "  <div id='navigation'>\n";
   print OUTPUT "   <h2>Navigation</h2>\n";
   print OUTPUT "   <ul>\n";

   print OUTPUT "     <li><a href='#Description' onclick='return switchPage(\"page1\")'>Description</a></li>\n"
      if (defined $tags{"description"});
   print OUTPUT "     <li><a href='#Types' onclick='return switchPage(\"page1\")'>Types</a></li>\n"
      if ($has_types);
   print OUTPUT "     <li><a href='#Subprograms' onclick='return switchPage(\"page1\")'>Subprograms</a>\n";
   print OUTPUT "       <ul>\n";
   foreach (@sections) {
      my ($current, $section) = ($_->[0], $_->[0]);
      $section =~ s/[ \t]/_/g;
      print OUTPUT "        <li><a href='#Subprograms__$section' onclick='return switchPage(\"page1\")'>",
                   $current,
                   "</a></li>\n";

   }
   print OUTPUT "       </ul>\n";
   print OUTPUT "   </li>\n";

   print OUTPUT "     <li><a href='#Signals' onclick='return switchPage(\"page2\")'>Signals</a></li>\n"
      if (defined $tags{"signals"});
   print OUTPUT "     <li><a href='#Properties' onclick='return switchPage(\"page3\")'>Properties</a></li>\n"
      if (defined $tags{"properties"});
   print OUTPUT "     <li><a href='#StyleProperties' onclick='return switchPage(\"page3\")'>Style Properties</a></li>\n"
      if (defined $tags{"style_properties"});
   print OUTPUT "     <li><a href='#ChildProperties' onclick='return switchPage(\"page3\")'>Child Properties</a></li>\n"
      if (defined $tags{"child_properties"});
   print OUTPUT "     <li><a href='#Examples' onclick='return switchPage(\"page4\")'>Examples</a></li>\n"
      if (defined $tags{"example"});
   print OUTPUT "     <li><a href='#Testgtk' onclick='return switchPage(\"page5\")'>Testgtk</a></li>\n"
      if (defined $tags{"testgtk"});

   print OUTPUT "   </ul>\n";
   print OUTPUT "  </div> <!--  navigation -->\n\n";

   ## See also
   if (defined $tags{"see"}) {
      print OUTPUT "  <div id='seeAlso'>\n";
      print OUTPUT "   <h2>See Also</h2>\n";
      print OUTPUT "   <ul>\n";
      foreach $w (split ("\n", $tags{'see'})) {
         my ($file) = $files_from_package{$w};
         if (defined $file) {
            print OUTPUT "     <li><a href='$file'>$w</a></li>\n";
         }
      }
      print OUTPUT "   </ul>\n";
      print OUTPUT "  </div>  <!-- seeAlso --> \n\n";
   }

   ## Finish left side
   print OUTPUT "</div>  <!--  leftSide -->\n\n";

   ## Documentation
   print OUTPUT "<div id='documentation'>\n";

   ## Notebook
   print OUTPUT "  <ul id='notebook'>\n";
   print OUTPUT "   <li id='tab_page1' class='current'><a href='' ",
                "onclick='return !switchPage(\"page1\")'>Entities</a></li>\n";
   print OUTPUT "   <li id='tab_page2'><a href='#Signals' ",
                "onclick='return !switchPage(\"page2\")'>Signals</a></li>\n"
      if (defined $tags{"signals"});
   print OUTPUT "   <li id='tab_page3'><a href='#Properties' ",
                "onclick='return !switchPage(\"page3\")'>Properties</a></li>\n"
      if (defined $tags{"properties"}
          || defined $tags{"style_properties"}
          || defined $tags{"child_properties"});
   print OUTPUT "  <li id='tab_page4'><a href='#Examples' ",
                "onclick='return !switchPage(\"page4\")'>Examples</a></li>\n"
      if (defined $tags{"example"});
   print OUTPUT "  <li id='tab_page5'><a href='#Testgtk' ",
                "onclick='return !switchPage(\"page5\")'>Testgtk</a></li>\n"
      if (defined $tags{"testgtk"});
   print OUTPUT "  </ul>  <!-- notebook --> \n\n";

   ## First notebook page
   print OUTPUT "  <div id='notebook_page1' class='notebookPage'>\n";

   ## Description of package
   if (defined $tags{'description'}) {
      print OUTPUT "  <a name='Description'></a>\n";
      print OUTPUT "  <div class='description'>\n";
      print OUTPUT "   <h2>Description</h2>\n";
      print OUTPUT &process_comment ($tags{'description'}, $package);
      print OUTPUT "  </div> <!-- description -->\n\n";
   }

   ## Page1 => Types
   if ($has_types) {
      print OUTPUT "  <a name='Types'></a>\n";
      print OUTPUT "  <div id='types'>\n";
      print OUTPUT "   <h2>Types</h2>\n";
      print OUTPUT "   <ul>\n";

      foreach (sort keys %types) {
        my ($name, $def, $comment) = ($_, $types{$_}->[0], $types{$_}->[1]);
        $def =~ s/</&lt;/g;  ## Think of  "type A (<>) is ..."
        $def =~ s/>/&gt;/g;
        print OUTPUT "     <li><a name='${name}_'></a>\n";
        print OUTPUT "         <div class='profile'>",
                     &process_profile($def),
                     "</div>\n";
        print OUTPUT "         <div class='comment'>",
                     &process_comment($comment, $package),
                     "</div></li>\n"; 
      }

      print OUTPUT "   </ul>\n";
      print OUTPUT "  </div> <!-- types -->\n\n";
   }

   ## Page1 => Subprograms
   print OUTPUT "  <a name='Subprograms'> </a>\n";
   print OUTPUT "  <a name='Subprograms__General'> </a>\n";
   print OUTPUT "  <div id='subprograms'>\n";
   print OUTPUT "   <h2>Subprograms</h2>\n";
   print OUTPUT "   <ul>\n";

   my (%names);
   my ($current_section) = "General";
   $count = 1;

   foreach $w (@subprograms) {
      my ($section, $description, $comment) = ($w->[0], $w->[1], $w->[2]);
      if ($section ne $current_section) {
         $current_section = $section;
         $section =~ s/[ \t]/_/g;

         print OUTPUT "  <a name='Subprograms__$section'></a>\n";
         print OUTPUT "  <h3>$current_section</h3>\n";

         foreach (@sections) {
            my ($name, $comment) = ($_->[0], $_->[1]);
            if ($name eq $current_section) {
               print OUTPUT "  <div class='description'>",
                      &process_comment ($comment, $package),
                      "</div>\n\n";
            }
         }
      }

      print OUTPUT "     <li>";
      my (%params) = ();
      while ($description =~ /($subprogram_re)/og) {
         my ($sname, $sprofile) = ($2, $1);

         # Output profile for each subprograms in the group
         print OUTPUT "<a name='${sname}_${count}_'></a>\n";
         print OUTPUT "<div class='profile'>",
                      &process_profile ($sprofile),
                      "</div>\n";

         # Save the name for the index
         $names{"${sname}_${count}_"} = $sname;
         $count++;
      }

      # Highlight parameters
      while ($description =~ /[(;]\s*(?:--)?\s*(\w+)\s*:/og) {
         my ($pname) = $1;
         $params{$pname}++;
      }

      # Output the common comment for all subprograms in the group
      print OUTPUT "<div class='comment'>",
                   &process_comment ($comment, $package, \%params),
                   "</div></li>\n";
   }

   print OUTPUT "    </ul>\n";
   print OUTPUT "   </div> <!--  subprograms -->\n\n";

   ## End of first notebook page
   print OUTPUT "  </div> <!--  notebook_page1 --> \n";

   ## Second notebook page (signals)
   if (defined $tags{'signals'}) {
      print OUTPUT "  <div id='notebook_page2' class='notebookPage'>\n";
      print OUTPUT "    <a name='Signals'></a>\n";
      print OUTPUT "    <div id='signals'>\n";
      print OUTPUT "      <h2>Signals</h2>\n";

      my (%signals) = &parse_signals ($tags{'signals'});
      print OUTPUT "   <ul>\n";
      foreach (sort keys %signals) {
         my ($name, $profile, $comment) = ($_, $signals{$_}->[0], $signals{$_}->[1]);
         print OUTPUT "    <li><div class='name'>$name</div>\n";
         print OUTPUT "        <div class='profile'>",
                      &process_profile ($profile),
                      "</div>\n";
         print OUTPUT "        <div class='comment'>",
                      &process_comment ($comment, $package),
                      "<div></li>\n";
      }
      print OUTPUT "   </ul>\n";

      print OUTPUT "    </div> <!-- signals -->\n";
      print OUTPUT "  </div> <!--  notebook_page2 -->\n\n";
   }

   ## Third notebook page (properties)
   my ($proptype);
   my (%properties_sections) =
       ('properties'       => 'Properties',
        'child_properties' => 'Child Properties',
        'style_properties' => 'Style Properties');
   print OUTPUT "  <div id='notebook_page3' class='notebookPage'>\n";
   foreach $proptype (('properties', 'child_properties', 'style_properties')) {
      if (defined $tags{$proptype}) {
         print OUTPUT "    <a name='$properties_sections{$proptype}'></a>\n";
         print OUTPUT "    <div class='properties'>\n";
         print OUTPUT "      <h2>$properties_sections{$proptype}</h2>\n";

         my (%props) = &parse_properties ($tags{$proptype});
         print OUTPUT "      <ul>\n";
         foreach (sort keys %props) {
            my ($name, $type, $descr, $see) = ($_, $props{$_}->[0],
                                               $props{$_}->[1],
                                               $props{$_}->[2],
                                               $props{$_}->[3]);
            print OUTPUT "        <li><div class='name'>$name</div>\n";
            print OUTPUT "            <div class='profile'>$type</div>\n"; 
            print OUTPUT "            <div class='comment'>",
                     (defined $descr ? &process_comment ($descr, $package) : ""),
                     (defined $see ? "<br><b>See:</b> " . &process_comment ($see, $package) : ""),
                     "</div></li>\n";
         }

         print OUTPUT "      </ul>\n";
         print OUTPUT "    </div> <!-- properties -->\n";
      }
   }
   print OUTPUT "  </div> <!-- notebook_page3 -->\n\n";

   ## Fourth page (example)
   if (defined $tags{'example'}) {
      print OUTPUT "  <div id='notebook_page4' class='notebookPage'>\n";
      print OUTPUT "   <a name='Example'></a>\n";
      print OUTPUT "   <div id='example'>";
      print OUTPUT "<h2>Example</h2>";
      my ($example) = $tags{'example'};
      if (($example =~ /<include>(.*)<\/include>/)) {
         my ($base, $dirname) = ($1, $filename);
         $dirname =~ s,/[^/]+$,/,;
         open (EXAMPLE, "$dirname$base") || print "Cannot open $dirname$base\n";
         $example = join ("", <EXAMPLE>);
         close (EXAMPLE);
      }

      $example =~ s/^\s*--//mg;
      print OUTPUT &highlight_syntax ($example);
      print OUTPUT "</div> <!-- example -->\n";
      print OUTPUT "  </div> <!-- notebook_page4 -->\n\n";
   }

   ## Fifth page (testgtk)
   if (defined $tags{'testgtk'}) {
      print OUTPUT "  <div id='notebook_page5' class='notebookPage'>\n";
      print OUTPUT "   <a name='Testgtk'></a>\n";
      print OUTPUT "   <div id='testgtk'>";
      print OUTPUT "<h2>Testgtk source code</h2>";
      print OUTPUT "<div class='description'>This code is part of testgtk, a demo application",
                   " packaged with GtkAda. Testgtk demonstrates the various",
                   " widgets of GtkAda</div>";
      open (TESTGTK, "../../testgtk/$tags{'testgtk'}") || print "Cannot open file testgtk file $tags{'testgtk'}\n";
      print OUTPUT &highlight_syntax (join ("", <TESTGTK>));
      close (TESTGTK);
      print OUTPUT "</div> <!-- testgtk -->\n";
      print OUTPUT "  </div> <!-- notebook_page4 -->\n\n";
   }

   ## Finish documentation
   print OUTPUT "</div> <!-- documentation -->\n\n";

   ## Start right side
   print OUTPUT "<div id='rightSide'>\n";
   print OUTPUT " <div id='Index'>\n";
   print OUTPUT "  <h2>Alphabetical Index</h2>\n";
   print OUTPUT "  <ul>\n";

  foreach $w (sort keys %names) {
      print OUTPUT "   <li><a href='#${w}' onclick='return switchPage(\"page1\")'>$names{$w}</a></li>\n";
   }

   print OUTPUT "  </ul>\n";
   print OUTPUT " </div> <!-- Index -->\n";
   print OUTPUT "</div> <!-- rightSide -->\n\n";

   print OUTPUT <<EOF
   <script language='javascript'>switchPage('page1');
adjust_height()</script>
 </body>
</html>
EOF
;

   close (OUTPUT);
}

#######################
## Generate the general index
#######################

sub generate_index() {
  my ($entity, $short);
  my ($first);
  my (%short_entities);

  open (OUTPUT, ">gtkada_rm/entities.html");
  &generate_header ("Index", *OUTPUT);

  print OUTPUT "<div id='IndexIndex'>\n";
  print OUTPUT " <h2>Index</h2>\n";
  print OUTPUT "<table>\n";
  print OUTPUT "  <tr><td colspan='3'><a href='#operators'>operators</a></td>\n";
  for ($first = ord ('a'); $first <= ord ('z'); $first++) {
     print OUTPUT "  <tr>\n" if (($first - ord ('a')) % 3 == 0);
     print OUTPUT "    <td><a href='#", chr ($first), "'>", uc (chr ($first)), "</a></td>\n";
     print OUTPUT "  </tr>\n" if (($first - ord ('a')) % 3 == 2);
  }
  print OUTPUT "</table>\n";
  print OUTPUT "</div> <!-- leftSide -->\n";

  foreach $entity (keys %entities) {
     my ($short) = $entity;
     $short =~ s/^.*\.([^\.]+)$/$1/;
     $short_entities{$short} = $entity;
  }

  $first = "";
  foreach $short (sort { lc($a) cmp lc($b) } keys %short_entities) {
     my ($entity) = $short_entities{$short};
     my ($html_file, $anchor) = @{$entities{$entity}};
     my ($package) = $entity;
     my ($new_first) = lc (substr ($short, 0, 1));
     $new_first = "operators" if ($new_first eq '"');

     if ($new_first ne $first) {
       print OUTPUT "</ul></div>\n" if ($first ne "");
       $first = $new_first;
       print OUTPUT "<div class='GeneralIndex'>\n";
       print OUTPUT " <a name='$first'></a>\n";
       print OUTPUT " <h2>$first</h2>\n";
       print OUTPUT "<ul>\n";
     } 

     $package =~ s/\.([^\.]+)$//;
     if ($anchor ne "") {
        print OUTPUT "   <li><a href='$html_file#$anchor'>$short</a> ($package)</li>\n";
     } else {
        print OUTPUT "   <li><a href='$html_file'>$short</a> ($package)</li>\n";
     }
  }

  print OUTPUT "</ul>\n";
  print OUTPUT "</div> <!-- GeneralIndex -->\n";
  print OUTPUT "</body></html>";
  close (OUTPUT);
}

#######################
## Generate the table of contents
#######################

sub generate_table_of_contents() {
   my ($group, $pkg);

   open (OUTPUT, ">gtkada_rm/index.html");
   &generate_header ("Reference Manual", *OUTPUT);

   foreach $group (sort keys %groups) {
      print OUTPUT "<div class='Toc'>\n"; 
      print OUTPUT "  <h2>$group</h2>\n";
      print OUTPUT "<ul>\n";
      foreach $pkg (sort @{$groups{$group}}) {
         print OUTPUT "  <li><a href='$files_from_package{$pkg}'>$pkg</a></li>\n";
      }
      print OUTPUT "</ul>\n";
      print OUTPUT "  </div> <!-- Toc -->\n";
   }

   print OUTPUT "</body></html>";

   close (OUTPUT);
}

#######################
## Generates the gallery
#######################

sub generate_gallery() {
  my ($screenshot, $pkg);

  open (OUTPUT, ">gtkada_rm/gallery.html");
  &generate_header ("Widgets Gallery", *OUTPUT);

  print OUTPUT "<div class='gallery-spacer' />\n";

  foreach $pkg (sort keys %screenshots) {
     $screenshot = $screenshots{$pkg};
     print OUTPUT "<div class='gallery'>\n";
     print OUTPUT "  <a href='$files_from_package{$pkg}' title='$pkg'>",
                  "<img src='$screenshot' alt='No image'/>",
                  "</a>\n";
     print OUTPUT "</div>\n";
  }

  print OUTPUT "<div class='gallery-spacer' />\n";
  print OUTPUT "</body></html>";
  close (OUTPUT);
}

#######################
## Generates the widget tree
#######################

sub print_children() {
  my ($level) = shift;
  my ($widget) = shift;
  my ($children) = shift;
  my (%children) = %$children;
  local (*OUTPUT) = shift;
  my ($has_next_child) = shift;
  my (@has_next_child) = @$has_next_child;  ## One entry for each level
  my ($do_xref) = shift;
  my (%do_xref) = %$do_xref; ## Whether we want xref for those widgets
  my ($count);

  print OUTPUT "  <li>";
  for ($count = 0; $count < $level - 1; $count++) {
     if ($has_next_child[$count]) {
        print OUTPUT "<img src='childtree4.png' alt='  '/>";
     } else {
        print OUTPUT "<img src='childtree2.png' alt='  '/>";
     }
  }

  if ($has_next_child[$level - 1]) {
     print OUTPUT "<img src='childtree3.png' alt='\_' />";
  } else {
     print OUTPUT "<img src='childtree.png' alt='\_' />";
  }

  if (defined $children{$widget}) {
     print OUTPUT "<a class='tree' onclick='treetoggle(this)'><img src='treeopen.png' alt='[-]' /></a>";
  }

  #if (defined $screenshots{$packages{$widget}}) {
  #   print OUTPUT " SCREENSHOT ";
  #}

  if (defined $do_xref{$widget} && $do_xref{$widget} == 0) {
     print OUTPUT "$widget</li>";
  } else {
     my ($tmp) = $files_from_widget{$widget};
     if (defined $tmp) {
         print OUTPUT "<a href='$tmp'>$widget</a></li>";
     }
  }

  if (defined $children{$widget}) {
     print OUTPUT "<ul>\n";
     my (@immediate) = @{$children{$widget}};
     while (@immediate) {
        $_ = shift @immediate;
        push (@has_next_child, $#immediate >= 0);
        &print_children ($level + 1, $_, \%children, *OUTPUT, \@has_next_child, \%do_xref);
        pop (@has_next_child);
     }
     print OUTPUT "</ul>\n";
  }
}

##############################
## Generate a widget tree for a given set of widgets
## If $xref_widgets_from_list is 1, then these widgets will also have hyper
## links, otherwise they don't
##############################

sub generate_tree_for_widgets() {
   my ($widget_list) = shift;
   my ($xref_widgets_from_list) = shift;
   local (*OUTPUT) = shift;
   my (@widget_list) = @$widget_list;
   my (%do_xref) = ();

   print OUTPUT "<ul class='top'>\n";

   ## Make sure the parents of each widget is in the list. This is wasted
   ## time when generating the whole inheritance tree, but doesn't really
   ## matter, that's fast enough
   my (%list);
   foreach (@widget_list) {
      $list{$_} ++;
      $do_xref{$_} = 0 if (!$xref_widgets_from_list);
      my ($w) = $parents{$_};
      while (defined $w && $w ne "") {
         $list{$w}++;
         $w = $parents{$w};
      }
   }

   my (%children);
   my (@root);
   my (@has_next_child);
   foreach (keys %list) {
      push (@root, $_) if (!defined $parents{$_} || $parents{$_} eq "");
      push (@{$children{$parents{$_}}}, $_) if (defined $parents{$_});
   }

   ## There could be several roots to the inheritance tree
   @root = sort @root;

   while (@root) {
      $_ = shift (@root);
      @has_next_child = ($#root >= 0);
      &print_children (1, $_, \%children, *OUTPUT, \@has_next_child, \%do_xref);
   }

   print OUTPUT "</ul>\n";
}

################################
## Generate the full inheritance tree
################################

sub generate_tree() {
   open (OUTPUT, ">gtkada_rm/tree.html");
   &generate_header ("Widgets Tree", *OUTPUT);

   print OUTPUT "<div id='widgetTreeButtons'>\n";
   print OUTPUT "<input type='button' onclick='treeChangeFoldAll(1)' value='fold all'/>\n";
   print OUTPUT "<input type='button' onclick='treeChangeFoldAll(0)' value='Expand all'/>\n";
   print OUTPUT "</div>\n";
   print OUTPUT "<div id='widgetTree'>\n";
   my (@list) = keys %parents;
   &generate_tree_for_widgets (\@list, 1, *OUTPUT);
   print OUTPUT "</div> <!-- widgetTree -->\n";
   print OUTPUT "</body></html>\n";
   close (OUTPUT);
}

#######################
## Main
#######################

## Parse all source files, to get info on type hierarchy

our ($source);
foreach $source (@source_files) {
   &extract_sections ($source);
}

## Then generate HTML for each source file
foreach $source (sort keys %files) {
   &generate_html ($source);
}

## Generate general files
&generate_table_of_contents();
&generate_index();
&generate_gallery();
&generate_tree();
