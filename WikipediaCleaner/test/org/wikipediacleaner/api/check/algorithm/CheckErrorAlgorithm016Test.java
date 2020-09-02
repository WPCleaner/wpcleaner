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
 * Test class for CW algorithm 2.
 */
public class CheckErrorAlgorithm016Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_016_prio_enwiki=3 END\r\n" + 
        " error_016_head_enwiki=Unicode control characters END\r\n" + 
        " error_016_desc_enwiki=The script found one or more invisible <a href=\"https://en.wikipedia.org/wiki/Unicode_control_characters\">Unicode control characters</a>. For example:<br>\r\n" + 
        " <a href=\"https://en.wikipedia.org/wiki/Zero-width_space\">Zero-width space</a> (U+200B), <a href=\"https://en.wikipedia.org/wiki/Left-to-right_mark\">left-to-right mark</a> (U+200E), <a href=\"https://en.wikipedia.org/wiki/Newline#Unicode\">line separator</a> (U+2028), <a href=\"https://en.wikipedia.org/wiki/Byte_order_mark\">byte order mark</a> (U+FEFF) or characters from <a href=\"https://en.wikipedia.org/wiki/Private_Use_Areas\">private use areas</a>. This could be a problem inside an article.<br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can correct the problem:\r\n" + 
        "<ul>\r\n" + 
        "<li><a href=\"https://meta.wikimedia.org/wiki/User:TMg/autoFormatter\">Auto-Formatter</a> (most errors)</li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/WP:AWB\">AutoWikiBrowser (AWB)</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:WPCleaner\">WPCleaner</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:AutoEd\">AutoEd</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_016_use_A0_enwiki=false END";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 16, 1000, 1000);
  }
}
