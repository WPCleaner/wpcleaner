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
 * Test class for CW algorithm 48.
 */
public class CheckErrorAlgorithm048Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_048_prio_enwiki=3 END\r\n" + 
        " error_048_head_enwiki=Title linked in text END\r\n" + 
        " error_048_whitelistpage_enwiki=Wikipedia:WikiProject_Check_Wikipedia/Error_048_whitelist END\r\n" + 
        " error_048_desc_enwiki=Found a link to the title inside the text. In the lead section of the article, please change this [[Title]] into '''Title''' (see also <a href=\"https://en.wikipedia.org/wiki/Wikipedia talk:WikiProject Check Wikipedia/Archive 1#Title in text\">Title in text</a>). However, do not make this change inside image maps, because doing so will cause errors. <br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can correct the problem:\r\n" + 
        "<ul>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/WP:AWB\">AutoWikiBrowser (AWB)</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:WPCleaner\">WPCleaner</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:AutoEd\">AutoEd</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_048_imagemap_enwiki=true END\r\n" + 
        "";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 48, 1000, 1000);
  }
}
