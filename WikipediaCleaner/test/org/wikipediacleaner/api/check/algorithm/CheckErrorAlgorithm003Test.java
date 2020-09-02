/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import java.io.StringReader;

import org.junit.Test;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.EnumWikipediaUtils;

/**
 * Test class for CW algorithm 3.
 */
public class CheckErrorAlgorithm003Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_003_prio_enwiki=1 END\r\n" + 
        " error_003_head_enwiki=Reference list missing END\r\n" + 
        " error_003_desc_enwiki=The article contains one or more &lt;ref&gt; tags, but is missing a reference list tag ({{reflist}} or &lt;references&nbsp;/&gt;). For details see <a href=\"https://en.wikipedia.org/wiki/Help:Footnotes\">Help:Footnotes</a>.<br>\r\n" + 
        "<br>\r\n" + 
        "The script recognizes most redirects to {{reflist}} on <a href=\"https://en.wikipedia.org/w/index.php?title=Special:WhatLinksHere/Template:Reflist&hidelinks=1&hidetrans=1\">Special:WhatLinksHere/Template:Reflist</a>.<br>\r\n" + 
        "<br>\r\n" + 
        "MediaWiki includes these articles into <a href=\"https://en.wikipedia.org/wiki/Category:Pages_with_incorrect_ref_formatting\">Category:Pages with incorrect ref formatting</a> and displays an error message on the page.<br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can warn you if there is problem:<ul>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/WP:AWB\">AutoWikiBrowser (AWB)</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:WPCleaner\">WPCleaner</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_003_templates_enwiki=\r\n" + 
        "   ref-list\r\n" + 
        "   refs\r\n" + 
        "   reference list\r\n" + 
        "   listaref\r\n" + 
        "   footnotes\r\n" + 
        "   list of botanists by author abbreviation footer\r\n" + 
        "   MinorPlanetNameMeaningsFooter00\r\n" + 
        "   MinorPlanetNameMeaningsFooter\r\n" + 
        "   MinorPlanetNameMeaningsFooter10k\r\n" + 
        "   SCOTUS-justice-listframe\r\n" + 
        "   Airline codes page/bottom\r\n" + 
        "   Ref list\r\n" + 
        "   RE\\s*[|}]\r\n" + 
        "   Reference\\s*[|}]\r\n" + 
        "   Schubert's compositions \\(references\\)\r\n" + 
        "   rfs END";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 3, 1000, 1000);
  }
}
