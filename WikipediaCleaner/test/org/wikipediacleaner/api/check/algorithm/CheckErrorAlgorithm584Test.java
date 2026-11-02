/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import static org.junit.Assert.assertEquals;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.junit.BeforeClass;
import org.junit.Test;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.EnumWikipediaUtils;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Tests for algorithm 584.
 */
public class CheckErrorAlgorithm584Test {

  private static EnumWikipedia wiki;

  private static CheckErrorAlgorithm algorithm;

  @BeforeClass
  public static void configure() throws APIException {
    wiki = EnumWikipediaUtils.getFR();
    String wpcConfiguration =
        "general_template_groups=\r\n" +
        "  Palette|Palette|Palettes\r\n" +
        "  Clear|Clear|Clr END";
    wiki.getConfiguration().setConfiguration(new StringReader(wpcConfiguration), null);
    String cwConfiguration =
        " error_584_prio_frwiki=2 END\r\n" +
        " error_584_templates_frwiki=\r\n" +
        "   +Palette END\r\n" +
        " error_584_line_jump_templates_frwiki=\r\n" +
        "   +Clear\r\n" +
        "   Saut END";
    wiki.getCWConfiguration().setWikiConfiguration(new StringReader(cwConfiguration));
    CheckErrorAlgorithms.initializeAlgorithms(wiki);
    algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, 584);
  }

  private void check(String text) {
    check(text, text, false);
  }

  private void check(String text, String expected, boolean automatic) {
    Page page = DataManager.createSimplePage(wiki, "Test", null, null, null);
    PageAnalysis analysis = new PageAnalysis(page, text);
    List<CheckErrorResult> errors = new ArrayList<>();
    algorithm.analyze(analysis, errors, false);
    boolean error = !text.equals(expected);
    assertEquals("Errors for " + text, error ? 1 : 0, errors.size());
    if (error) {
      CheckErrorResult errorResult = errors.get(0);
      String fixed =
          text.substring(0, errorResult.getStartPosition()) +
          errorResult.getFirstReplacement() +
          text.substring(errorResult.getEndPosition());
      assertEquals("Fix for " + text, expected, fixed);
    }
    assertEquals("Automatic fix for " + text, automatic ? expected : text, algorithm.automaticFix(analysis));
  }

  @Test
  public void testCorrect() {
    check("Texte.\n\n{{Palette|Thème}}");
    check("Texte.\n{{Palette|Thème}}");
    check("{{Palette|Thème}}");
    check("Texte.\n\n\n{{Autre}}");
    check("{{Boîte|\n\n\n{{Palette|Thème}}}}");
  }

  @Test
  public void testMultipleBlankLines() {
    check("Texte.\n\n\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", false);
    check("Texte.\n \n\n\n{{Palette}}", "Texte.\n\n{{Palette}}", false);
  }

  @Test
  public void testBrTag() {
    check("Texte.<br />\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
    check("Texte.\n<br clear=\"all\">\n\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
  }

  @Test
  public void testLineJumpTemplates() {
    check("Texte.\n{{Clr}}\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
    check("Texte.\n\n{{clear}}\n\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
    check("Texte.\n{{Saut|2em}}\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
    check("Texte.\n{{Clr}}{{Saut|10px}}\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
    check("Texte.<br />\n{{Clr}}\n{{Palette|Thème}}", "Texte.\n\n{{Palette|Thème}}", true);
  }

  @Test
  public void testCommentBefore() {
    check("Texte.<!-- Commentaire -->\n{{Clr}}\n{{Palette|Thème}}", "Texte.<!-- Commentaire -->\n\n{{Palette|Thème}}", false);
    check("Texte.\n<!-- Commentaire --> \n\n{{clear}}\n\n{{Palette|Thème}}", "Texte.\n<!-- Commentaire -->\n\n{{Palette|Thème}}", false);
    check("Texte.\n<!-- Commentaire -->\n\n\n{{Palette|Thème}}", "Texte.\n<!-- Commentaire -->\n\n{{Palette|Thème}}", false);
    check("<!-- Commentaire -->Texte.\n{{Clr}}\n{{Palette|Thème}}", "<!-- Commentaire -->Texte.\n\n{{Palette|Thème}}", true);
    check("Texte.<!-- Commentaire --><br />\n{{Palette|Thème}}", "Texte.<!-- Commentaire -->\n\n{{Palette|Thème}}", false);
  }

  @Test
  public void testTemplateGroups() {
    check("Texte.\n{{Clr}}\n{{Palettes|Thème}}", "Texte.\n\n{{Palettes|Thème}}", true);
    check("Texte.\n\n\n{{Palettes|Thème}}", "Texte.\n\n{{Palettes|Thème}}", false);
  }
}
