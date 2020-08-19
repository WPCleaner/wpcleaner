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
 * Test class for CW algorithm 28.
 */
public class CheckErrorAlgorithm061Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_061_prio_enwiki=3 END\r\n" + 
        " error_061_head_enwiki=Reference before punctuation END\r\n" + 
        " error_061_whitelistpage_enwiki=Wikipedia:WikiProject_Check_Wikipedia/Error_061_whitelist END\r\n" + 
        " error_061_desc_enwiki=The script found a punctuation (.,?:;!) after the reference. For example: \"&lt;/ref&gt;.\" - The punctuation should stand before the references. See [[<a href=\"https://en.wikipedia.org/wiki/Wikipedia:REFPUNC#Punctuation_and_footnotes\">WP:REFPUNC</a>]].<br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can correct the problems:\r\n" + 
        "<ul>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/WP:AWB\">AutoWikiBrowser (AWB)</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:WPCleaner\">WPCleaner</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_061_templates_enwiki=\r\n" + 
        "   sfn\r\n" + 
        "   sfnm\r\n" + 
        "   sfnp\r\n" + 
        "   rp END\r\n" + 
        "";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 61, 1000, 1000);
  }
}
