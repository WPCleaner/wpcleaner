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
public class CheckErrorAlgorithm002Test extends CheckErrorAlgorithmTest {

  /**
   * Test performance of algorithm.
   */
  @Test
  public void testPerformance() throws APIException {

    // Configure algorithm
    EnumWikipedia wiki = EnumWikipediaUtils.getEN();
    String cwConfiguration =
        " error_002_prio_enwiki=1 END\r\n" + 
        " error_002_head_enwiki=Tag with incorrect syntax END\r\n" + 
        " error_002_whitelistpage_enwiki=Wikipedia:WikiProject_Check_Wikipedia/Error_002_whitelist END\r\n" + 
        " error_002_desc_enwiki=The article contains one or more &lt;br&gt;, &lt;center&gt; or &lt;small&gt; tags with incorrect syntax. Also checks &lt;span/&gt; and &lt;div/&gt;, which are incorrect HTML5. For example:\r\n" + 
        "<ul>\r\n" + 
        "<li>&lt;/br&gt; (slash in front instead of the end)</li>\r\n" + 
        "<li>&lt;br&#92;&gt; (backslash instead of slash)</li>\r\n" + 
        "<li>&lt;br.&gt;</li>\r\n" + 
        "</ul>\r\n" + 
        "Use &lt;br&gt; (<a href=\"http://www.w3.org/TR/html5/text-level-semantics.html#the-br-element\">HTML5</a>) or &lt;br /&gt; (<a href=\"http://www.w3schools.com/tags/tag_br.asp\">XHTML</a>) or remove the tag. Instead of the obsolete \"clear\" attribute use the {{clear}} template. See <a href=\"https://en.wikipedia.org/wiki/Wikipedia:HTML5#Other_obsolete_attributes\">Wikipedia:HTML5#Other obsolete attributes</a>.<br>\r\n" + 
        "<br>\r\n" + 
        "Following tools can correct the problem:\r\n" + 
        "<ul>\r\n" + 
        "<li><a href=\"https://meta.wikimedia.org/wiki/User:TMg/autoFormatter\">Auto-Formatter</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/WP:AWB\">AutoWikiBrowser (AWB)</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:WPCleaner\">WPCleaner</a></li>\r\n" + 
        "<li><a href=\"https://en.wikipedia.org/wiki/Wikipedia:AutoEd\">AutoEd</a></li>\r\n" + 
        "</ul> END\r\n" + 
        " error_002_anchor_templates_enwiki=\r\n" + 
        "   Anchor|1 END\r\n" + 
        " error_002_clear_all_enwiki={{Clear}} END\r\n" + 
        " error_002_clear_left_enwiki={{Clear|left}} END\r\n" + 
        " error_002_clear_right_enwiki={{Clear|right}} END";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));

    // Test
    testPerformance(wiki, "PageAnalysisTest_en_Windows_10_version_history", 2, 1000, 1000);
  }
}
