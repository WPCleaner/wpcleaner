/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.analysis.InternalLinkCount;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Test class for PageUtilities.
 */
public class PageUtilitiesTest {

  String pageText;

  @Before
  public void beforeTest() {
    File testFile = new File("test/org/wikipediacleaner/api/data/PageUtilitiesTest_1.txt");
    try {
      pageText = FileUtils.readFileToString(testFile, StandardCharsets.UTF_8);
    } catch (FileNotFoundException e) {
      fail("Unable to open test file: " + testFile.getAbsolutePath());
    } catch (IOException e) {
      fail("Error reading file: " + testFile + "\n" + e.getMessage());
    }
  }
  
  @After
  public void afterTest() {
    pageText = null;
  }
  
  @Test
  public void testCountLinkOccurencesInText() {
    Page page = DataManager.getPage(EnumWikipedia.FR, "Utilisateur:Salebot/Journal/2008-11-05", null, null, null);
    Page link = DataManager.getPage(EnumWikipedia.FR, "AFP", null, null, null);
    PageAnalysis pageAnalysis = page.getAnalysis(pageText, true);
    pageAnalysis.countLinks(Collections.singletonList(link));
    InternalLinkCount count = pageAnalysis.getLinkCount(link);
    assertNotNull(count);
    assertEquals(1, count.getTotalLinkCount());
  }
}
