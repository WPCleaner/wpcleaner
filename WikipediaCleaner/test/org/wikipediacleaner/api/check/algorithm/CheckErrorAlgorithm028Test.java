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
public class CheckErrorAlgorithm028Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_028_prio_enwiki=1 END\r\n" + 
        " error_028_head_enwiki=Table without correct end END\r\n" + 
        " error_028_whitelistpage_enwiki=Wikipedia:WikiProject_Check_Wikipedia/Error_028_whitelist END\r\n" + 
        " error_028_desc_enwiki=Found no end of the table. \"|}\" generally needs to be added. MediaWiki generally displays the table despite the missing end, but content below the table gets mixed into the table. Where an infobox is more suitable than the table, the article's talk page could be tagged with {{newinfobox}}.<br>\r\n" + 
        "<br>\r\n" + 
        "Different markup for start and end : If wiki syntax \"{|\" is used to start the table, the end tag should be \"|}\".  There are cases were a template can end a table, such as {{fb end}}. This is perfectly fine to do.<br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can warn of a problem:\r\n" + 
        "<ul>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/WP:AWB\">AutoWikiBrowser (AWB)</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:WPCleaner\">WPCleaner</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_028_templates_enwiki=\r\n" + 
        "   {{CBB Schedule End}}\r\n" + 
        "   {{CBB yearly record end\r\n" + 
        "   {{Election box end\r\n" + 
        "   {{end}}\r\n" + 
        "   {{End box}}\r\n" + 
        "   {{End table}}\r\n" + 
        "   {{Fb cs footer\r\n" + 
        "   {{Fb cl footer\r\n" + 
        "   {{Fb disc footer\r\n" + 
        "   {{Fb footer\r\n" + 
        "   {{Fb kit footer\r\n" + 
        "   {{Fb match footer\r\n" + 
        "   {{Fb oi footer\r\n" + 
        "   {{fb overall competition footer\r\n" + 
        "   {{Fb r footer\r\n" + 
        "   {{Fb rbr pos footer\r\n" + 
        "   {{Fs end\r\n" + 
        "   {{Ig footer\r\n" + 
        "   {{Irish MEP table end}}\r\n" + 
        "   {{Jctbtm\r\n" + 
        "   {{LegendRJL\r\n" + 
        "   {{NBA roster footer\r\n" + 
        "   {{PBA roster footer\r\n" + 
        "   {{PHL sports results footer\r\n" + 
        "   {{S-end}}\r\n" + 
        "   {{WNBA roster footer END";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 28, 1000, 1000);
  }
}
