/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.analysis;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;
import org.wikipediacleaner.api.constants.EnumWikipediaUtils;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.api.data.contents.comment.ContainerComment;


/**
 * Test class for page analysis.
 */
public class PageAnalysisTest {

  /**
   * Test on a simple page.
   */
  @Test
  public void testSimplePage() {

    // Create contents and analysis
    PageAnalysis analysis = PageAnalysisUtils.analyzeAndTestPage(
        EnumWikipediaUtils.getEN(), "PageAnalysisTest_1");

    // Check elements
    checkComments(analysis, 1);
    checkTags(analysis, 5);
    checkTags(analysis, PageElementTag.TAG_HTML_DIV, 2);
    checkTags(analysis, PageElementTag.TAG_WIKI_NOWIKI, 3);
    checkInternalLinks(analysis, 2);
    checkImages(analysis, 2);
    checkCategories(analysis, 2);
    checkInterwikiLinks(analysis, 2);
    checkLanguageLinks(analysis, 1);
    checkFunctions(analysis, 1);
    checkMagicWords(analysis, 1);
    checkTemplates(analysis, 3);
    checkParameters(analysis, 2);
    checkTitles(analysis, 2);
    checkExternalLinks(analysis, 2);
    checkISBN(analysis, 1);
    checkISSN(analysis, 1);
    checkPMID(analysis, 1);
    checkRFC(analysis, 1);
    checkTables(analysis, 1);
    checkListItems(analysis, 3);
    checkParagraphs(analysis, 17);
  }

  /**
   * Test on a big page from English wikipedia.
   */
  @Test
  public void testEn_2020_in_science() {

    // Create contents and analysis
    PageAnalysis analysis = PageAnalysisUtils.analyzeAndTestPage(
        EnumWikipediaUtils.getEN(), "PageAnalysisTest_en_2020_in_science");

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
   * Test on a big page from English wikipedia.
   */
  @Test
  public void testEn_Windows_10_version_history() {

    // Create contents and analysis
    PageAnalysis analysis = PageAnalysisUtils.analyzeAndTestPage(
        EnumWikipediaUtils.getEN(), "PageAnalysisTest_en_Windows_10_version_history");

    // Check elements
    checkComments(analysis, 1);
    checkTags(analysis, 3801);
    checkTags(analysis, PageElementTag.TAG_HTML_SMALL, 4);
    checkInternalLinks(analysis, 649);
    checkImages(analysis, 0);
    checkCategories(analysis, 4);
    checkInterwikiLinks(analysis, 0);
    checkLanguageLinks(analysis, 0);
    checkFunctions(analysis, 0);
    checkMagicWords(analysis, 0);
    checkTemplates(analysis, 1120);
    checkParameters(analysis, 0);
    checkTitles(analysis, 18);
    checkExternalLinks(analysis, 1437);
    checkISBN(analysis, 0);
    checkISSN(analysis, 0);
    checkPMID(analysis, 0);
    checkRFC(analysis, 0);
    checkTables(analysis, 23);
    checkListItems(analysis, 594);
    checkParagraphs(analysis, 205);
  }
  /**
   * Test on a big page from French wikipedia.
   */
  @Test
  public void testFr_Liste_des_noms_de_famille_basques() {

    // Create contents and analysis
    PageAnalysis analysis = PageAnalysisUtils.analyzeAndTestPage(
        EnumWikipediaUtils.getFR(), "PageAnalysisTest_fr_Liste_des_noms_de_famille_basques");

    // Check elements
    checkComments(analysis, 2);
    checkTags(analysis, 20);
    checkTags(analysis, PageElementTag.TAG_HTML_SMALL, 2);
    checkInternalLinks(analysis, 20941);
    checkImages(analysis, 0);
    checkCategories(analysis, 2);
    checkInterwikiLinks(analysis, 0);
    checkLanguageLinks(analysis, 0);
    checkFunctions(analysis, 0);
    checkMagicWords(analysis, 0);
    checkTemplates(analysis, 604);
    checkParameters(analysis, 0);
    checkTitles(analysis, 24);
    checkExternalLinks(analysis, 5);
    checkISBN(analysis, 4);
    checkISSN(analysis, 0);
    checkPMID(analysis, 0);
    checkRFC(analysis, 0);
    checkTables(analysis, 0);
    checkListItems(analysis, 10838);
    checkParagraphs(analysis, 49);
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
        "List of " + name + " doesn't have " + expectedCount + " " + name,
        expectedCount, list.size());
  }
}
