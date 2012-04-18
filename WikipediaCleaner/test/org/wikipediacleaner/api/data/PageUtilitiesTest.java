/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.data;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Collection;
import java.util.Collections;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Test class for PageUtilities.
 */
public class PageUtilitiesTest {

  String pageText;

  @Before
  public void beforeTest() {
    Reader reader = null;
    File testFile = new File("test/org/wikipediacleaner/api/data/PageUtilitiesTest_1.txt");
    try {
      reader = new BufferedReader(new FileReader(testFile));
      StringBuilder buffer = new StringBuilder((int) (testFile.exists() ? testFile.length() : 1000));
      int caracter;
      while ((caracter = reader.read()) != -1) {
        buffer.append(Character.toChars(caracter));
      }
      pageText = buffer.toString();
    } catch (FileNotFoundException e) {
      fail("Unable to open test file: " + testFile.getAbsolutePath());
    } catch (IOException e) {
      fail("Error reading file: " + testFile + "\n" + e.getMessage());
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          //
        }
        reader = null;
      }
    }
  }
  
  @After
  public void afterTest() {
    pageText = null;
  }
  
  @Test
  public void testCountLinkOccurencesInText() {
    Page page = DataManager.getPage(EnumWikipedia.FR, "Utilisateur:Salebot/Journal/2008-11-05", null, null);
    Page link = DataManager.getPage(EnumWikipedia.FR, "AFP", null, null);
    Collection<PageElementComment> comments = PageContents.findAllComments(EnumWikipedia.FR, pageText);
    PageContents.countInternalLinks(EnumWikipedia.FR, page, pageText, comments, Collections.singletonList(link));
    assertEquals(1, link.getCountOccurrence());
  }
}
