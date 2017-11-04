package wikipedia

import java.io.File

object WikipediaData {
  
  /**
   * Return path to wikipedia data dump ~144 Mb.
   */
  private [wikipedia] def filePath = {
    val cl = this.getClass.getClassLoader
    val rsc = cl.getResource("wikipedia/wikipedia.dat")
    if (rsc == null) 
      sys.error("Please download the dataset as explained in the assignment instructions")
    new File(rsc.toURI).getPath
  }
  
  /**
   * Parse the wikipedia data dump file. Fetch the article without text tag
   */
  private [wikipedia] def parse(line: String): WikipediaArticle = {
    val subs = "</title><text>"
    val i = line.indexOf(subs)
    //
    val title = line.substring(14, i) // title
    val text  = line.substring(i + subs.length, line.length-16) // text    
    // line into text and title
    WikipediaArticle(title, text)
  }


  
}
