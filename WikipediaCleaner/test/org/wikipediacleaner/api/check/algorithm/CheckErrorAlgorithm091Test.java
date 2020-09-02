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
 * Test class for CW algorithm 91.
 */
public class CheckErrorAlgorithm091Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_091_prio_enwiki=2 END\r\n" + 
        " error_091_head_enwiki=Interwiki link written as an external link or used as a reference. END\r\n" + 
        " error_091_whitelistpage_enwiki=Wikipedia:WikiProject_Check_Wikipedia/Error_091_whitelist END\r\n" + 
        " error_091_desc_enwiki=The script finds an external link that should be replaced with a interwiki link.  An example would be on enwiki [http://fr.wikipedia.org/wiki/Larry_Wall Larry Wall] should be written as [[:fr:Larry Wall]]. Script also finds references that use Wikipedia as a source.<br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can correct the problem:\r\n" + 
        "<ul>\r\n" + 
        "<li><a href=\"https://meta.wikimedia.org/wiki/User:TMg/autoFormatter\">Auto-Formatter</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_091_link_templates_enwiki=\r\n" + 
        "   Cite web|url|title END";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 91, 1000, 1000);
  }
}
