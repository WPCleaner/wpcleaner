/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.analysis;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Test;
import org.wikipediacleaner.api.constants.EnumCaseSensitiveness;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.contents.ContainerComment;
import org.wikipediacleaner.api.data.contents.ContentsElement;


/**
 * Test class for comments inside contents.
 */
public class PageAnalysisTest {

  /**
   * Test on a simple page.
   */
  @Test
  public void testSimplePage() {

    // Create contents and analysis
    PageAnalysis analysis = analyzeAndTestPage("PageAnalysisTest_1");

    // Check elements
    checkComments(analysis, 1);
    checkTags(analysis, 5);
    checkTags(analysis, PageElementTag.TAG_HTML_DIV, 2);
    checkTags(analysis, PageElementTag.TAG_WIKI_NOWIKI, 3);
    checkInternalLinks(analysis, 2);
    checkImages(analysis, 2);
    checkCategories(analysis, 2);
    checkInterwikiLinks(analysis, 2);
    checkLanguageLinks(analysis, 0);
    checkFunctions(analysis, 0);
    checkMagicWords(analysis, 0);
    checkTemplates(analysis, 0);
    checkParameters(analysis, 0);
    checkTitles(analysis, 0);
    checkExternalLinks(analysis, 0);
    checkISBN(analysis, 0);
    checkISSN(analysis, 0);
    checkPMID(analysis, 0);
    checkRFC(analysis, 0);
    checkTables(analysis, 0);
    checkListItems(analysis, 0);
    checkParagraphs(analysis, 8);
  }

  /**
   * Test on a big page from English wikipedia.
   */
  @Test
  public void testBigPageEn() {

    // Create contents and analysis
    PageAnalysis analysis = analyzeAndTestPage("PageAnalysisTest_en_2020_in_science");

    // Check elements
    checkComments(analysis, 5);
    checkTags(analysis, 1958);
    checkTags(analysis, PageElementTag.TAG_HTML_SMALL, 40);
    checkInternalLinks(analysis, 1899);
    checkImages(analysis, 53);
    checkCategories(analysis, 6);
    checkInterwikiLinks(analysis, 0);
    checkLanguageLinks(analysis, 0);
    checkFunctions(analysis, 0);
    checkMagicWords(analysis, 0);
    checkTemplates(analysis, 1013);
    checkParameters(analysis, 0);
    checkTitles(analysis, 15);
    checkExternalLinks(analysis, 702);
    checkISBN(analysis, 0);
    checkISSN(analysis, 76);
    checkPMID(analysis, 106);
    checkRFC(analysis, 0);
    checkTables(analysis, 0);
    checkListItems(analysis, 779);
    checkParagraphs(analysis, 51);
  }

  /**
   * Perform an analysis and some global tests.
   * 
   * @param fileName File name.
   * @return Page analysis.
   */
  private PageAnalysis analyzeAndTestPage(String fileName) {

    // Configure wiki
    EnumWikipedia wiki = EnumWikipedia.EN;
    List<Namespace> namespaces = new ArrayList<>();
    namespaces.add(new Namespace(
        Integer.toString(Namespace.CATEGORY),
        "Category", "Category",
        EnumCaseSensitiveness.FIRST_LETTER, true));
    namespaces.add(new Namespace(
        Integer.toString(Namespace.IMAGE),
        "File", "File",
        EnumCaseSensitiveness.FIRST_LETTER, true));
    wiki.getWikiConfiguration().setNamespaces(namespaces);
    List<Interwiki> interwikis = new ArrayList<>();
    interwikis.add(new Interwiki("en", true, "en", "https://en.wikipedia.org"));
    interwikis.add(new Interwiki("fr", true, "fr", "https://fr.wikipedia.org"));
    wiki.getWikiConfiguration().setInterwikis(interwikis);

    // Create contents and analysis
    String text = readFile(fileName + ".txt");
    Page testPage = DataManager.getPage(wiki, fileName, null, null, null);
    PageAnalysis analysis = new PageAnalysis(testPage, text);
    AnalysisPerformance perf = new AnalysisPerformance();
    analysis.performFullPageAnalysis(perf);

    // Display performance
    System.out.println(fileName + ": " + perf.toMilliSeconds());

    return analysis;
  }

  /**
   * Check comments.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkComments(PageAnalysis analysis, int expectedCount) {
    ContainerComment commentsContainer = analysis.comments();
    assertNotNull(
        "Comments container is null",
        commentsContainer);
    checkList(commentsContainer.getAll(), "comments", expectedCount);
  }

  /**
   * Check tags.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkTags(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getTags(), "tags", expectedCount);
  }

  /**
   * Check tags.
   * 
   * @param analysis Page analysis.
   * @param tagName Name of the tag.
   * @param expectedCount Expected number of elements.
   */
  private void checkTags(PageAnalysis analysis, String tagName, int expectedCount) {
    checkList(analysis.getTags(tagName), "tags " + tagName, expectedCount);
  }

  /**
   * Check internal links.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkInternalLinks(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getInternalLinks(), "internal links", expectedCount);
  }

  /**
   * Check images.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkImages(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getImages(), "images", expectedCount);
  }

  /**
   * Check categories.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkCategories(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getCategories(), "categories", expectedCount);
  }

  /**
   * Check interwiki links.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkInterwikiLinks(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getInterwikiLinks(), "interwiki links", expectedCount);
  }

  /**
   * Check language links.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkLanguageLinks(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getLanguageLinks(), "language links", expectedCount);
  }

  /**
   * Check functions.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkFunctions(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getFunctions(), "functions", expectedCount);
  }

  /**
   * Check magic words.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkMagicWords(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getMagicWords(), "magic words", expectedCount);
  }

  /**
   * Check templates.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkTemplates(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getTemplates(), "templates", expectedCount);
  }

  /**
   * Check parameters.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkParameters(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getParameters(), "parameters", expectedCount);
  }

  /**
   * Check titles.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkTitles(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getTitles(), "titles", expectedCount);
  }

  /**
   * Check external links.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkExternalLinks(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getExternalLinks(), "external links", expectedCount);
  }

  /**
   * Check ISBN.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkISBN(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getISBNs(), "ISBNs", expectedCount);
  }

  /**
   * Check ISSN.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkISSN(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getISSNs(), "ISSNs", expectedCount);
  }

  /**
   * Check PMID.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkPMID(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getPMIDs(), "PMIDs", expectedCount);
  }

  /**
   * Check RFC.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkRFC(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getRFCs(), "RFCs", expectedCount);
  }

  /**
   * Check tables.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkTables(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getTables(), "tables", expectedCount);
  }

  /**
   * Check list items.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkListItems(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getListItems(), "list items", expectedCount);
  }

  /**
   * Check paragraphs.
   * 
   * @param analysis Page analysis.
   * @param expectedCount Expected number of elements.
   */
  private void checkParagraphs(PageAnalysis analysis, int expectedCount) {
    checkList(analysis.getParagraphs(), "paragraphs", expectedCount);
  }

  /**
   * Check a type of elements.
   * 
   * @param list List of elements.
   * @param name Name of the type of elements.
   * @param expectedCount Expected number of elements.
   */
  private void checkList(
      List<? extends ContentsElement> list,
      String name,
      int expectedCount) {
    assertNotNull(
        "List of " + name + " is null",
        list);
    assertEquals(
        "List of " + name + " doesn't have" + expectedCount + " " + name,
        expectedCount, list.size());
  }

  /**
   * Read a test file.
   * 
   * @param fileName File name.
   * @return Contents of the test file.
   */
  private String readFile(String fileName) {
    File testFile = new File("test/org/wikipediacleaner/api/data/analysis/" + fileName);
    try {
      return FileUtils.readFileToString(testFile, StandardCharsets.UTF_8);
    } catch (FileNotFoundException e) {
      fail("Unable to open test file: " + testFile.getAbsolutePath());
    } catch (IOException e) {
      fail("Error reading file: " + testFile + "\n" + e.getMessage());
    }
    return null;
  }
}
