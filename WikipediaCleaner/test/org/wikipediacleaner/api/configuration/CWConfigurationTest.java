/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2026  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.configuration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.StringReader;

import org.junit.Test;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;

/**
 * Tests for {@link CWConfiguration}.
 */
public class CWConfigurationTest {

  /**
   * Parameters properly terminated by END.
   *
   * @throws APIException Error parsing the configuration.
   */
  @Test
  public void testWithEnd() throws APIException {
    CWConfiguration config = new CWConfiguration("frwiki", EnumWikipedia.FR);
    config.setWikiConfiguration(new StringReader(
        " error_559_incorrect_separators_frwiki=\n" +
        "  {{exp|,}}\n" +
        "  et\n" +
        " END\n" +
        " error_559_templates_frwiki=\n" +
        "  Note\n" +
        "  Sfn\n" +
        " END\n"));
    CWConfigurationError error = config.getErrorConfiguration(559);
    assertNotNull(error);
    assertEquals(
        "{{exp|,}}\n  et",
        error.getSpecificProperty("incorrect_separators", true, false, false, false));
    assertEquals(
        "Note\n  Sfn",
        error.getSpecificProperty("templates", true, false, false, false));
  }

  /**
   * Missing END: the next parameter must not be swallowed.
   *
   * @throws APIException Error parsing the configuration.
   */
  @Test
  public void testMissingEnd() throws APIException {
    CWConfiguration config = new CWConfiguration("frwiki", EnumWikipedia.FR);
    config.setWikiConfiguration(new StringReader(
        " error_559_separator_frwiki={{,}} END\n" +
        " error_559_incorrect_separators_frwiki=\n" +
        "  {{exp|,}}\n" +
        "  et\n" +
        " error_559_templates_frwiki=\n" +
        "  Note\n" +
        "  Sfn\n" +
        " END\n" +
        "# error_560_link_frwiki=P:CS/560\n" +
        " error_560_prio_frwiki=3\n" +
        "error_561_prio_frwiki=2 END\n"));
    CWConfigurationError error = config.getErrorConfiguration(559);
    assertNotNull(error);
    assertEquals("{{,}}", error.getSpecificProperty("separator", true, false, false, false));
    assertEquals(
        "{{exp|,}}\n  et",
        error.getSpecificProperty("incorrect_separators", true, false, false, false));
    assertEquals(
        "Note\n  Sfn",
        error.getSpecificProperty("templates", true, false, false, false));
    CWConfigurationError error560 = config.getErrorConfiguration(560);
    assertNotNull(error560);
    assertEquals(3, error560.getPriority());
    CWConfigurationError error561 = config.getErrorConfiguration(561);
    assertNotNull(error561);
    assertEquals(2, error561.getPriority());
  }
}
