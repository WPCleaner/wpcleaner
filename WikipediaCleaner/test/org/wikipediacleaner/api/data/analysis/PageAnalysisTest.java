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
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementListItem;
import org.wikipediacleaner.api.data.PageElementMagicWord;
import org.wikipediacleaner.api.data.PageElementPMID;
import org.wikipediacleaner.api.data.PageElementParagraph;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementRFC;
import org.wikipediacleaner.api.data.PageElementTable;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.contents.ContainerComment;


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

    // Check comments
    ContainerComment commentContainer = analysis.comments();
    assertEquals(
        "List of comments doesn't have 1 comment",
        1, commentContainer.getAll().size());

    // Check tags
    List<PageElementTag> tags = analysis.getTags();
    assertEquals(
        "List of tags doesn't have 5 tags",
        5, tags.size());

    // Check internal links
    List<PageElementInternalLink> internalLinks = analysis.getInternalLinks();
    assertEquals(
        "List of internal links doesn't have 2 internal links",
        2, internalLinks.size());

    // Check images
    List<PageElementImage> images = analysis.getImages();
    assertEquals(
        "List of images doesn't have 2 images",
        2, images.size());

    // Check categories
    List<PageElementCategory> categories = analysis.getCategories();
    assertEquals(
        "List of categories doesn't have 2 categories",
        2, categories.size());

    // Check interwiki links
    List<PageElementInterwikiLink> iwLinks = analysis.getInterwikiLinks();
    assertEquals(
        "List of interwiki links doesn't have 2 interwiki links",
        2, iwLinks.size());

    // Check language links
    List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
    assertEquals(
        "List of language links doesn't have 0 language links",
        0, langLinks.size());

    // Check functions
    List<PageElementFunction> functions = analysis.getFunctions();
    assertEquals(
        "List of functions doesn't have 0 functions",
        0, functions.size());

    // Check magic words
    List<PageElementMagicWord> magicWords = analysis.getMagicWords();
    assertEquals(
        "List of magic words doesn't have 0 magic words",
        0, magicWords.size());

    // Check templates
    List<PageElementTemplate> templates = analysis.getTemplates();
    assertEquals(
        "List of templates doesn't have 0 templates",
        0, templates.size());

    // Check parameters
    List<PageElementParameter> parameters = analysis.getParameters();
    assertEquals(
        "List of parameters doesn't have 0 parameters",
        0, parameters.size());

    // Check titles
    List<PageElementTitle> titles = analysis.getTitles();
    assertEquals(
        "List of titles doesn't have 0 titles",
        0, titles.size());

    // Check external links
    List<PageElementExternalLink> eLinks = analysis.getExternalLinks();
    assertEquals(
        "List of external links doesn't have 0 external links",
        0, eLinks.size());

    // Check ISBN
    List<PageElementISBN> isbns = analysis.getISBNs();
    assertEquals(
        "List of ISBN doesn't have 0 ISBN",
        0, isbns.size());

    // Check ISSN
    List<PageElementISSN> issns = analysis.getISSNs();
    assertEquals(
        "List of ISSN doesn't have 0 ISSN",
        0, issns.size());

    // Check PMID
    List<PageElementPMID> pmids = analysis.getPMIDs();
    assertEquals(
        "List of PMID doesn't have 0 PMID",
        0, pmids.size());

    // Check RFC
    List<PageElementRFC> rfcs = analysis.getRFCs();
    assertEquals(
        "List of RFC doesn't have 0 RFC",
        0, rfcs.size());

    // Check tables
    List<PageElementTable> tables = analysis.getTables();
    assertEquals(
        "List of tables doesn't have 0 tables",
        0, tables.size());

    // Check list items
    List<PageElementListItem> listItems = analysis.getListItems();
    assertEquals(
        "List of list items doesn't have 0 list items",
        0, listItems.size());

    // Check paragraphs
    List<PageElementParagraph> paragraphs = analysis.getParagraphs();
    assertEquals(
        "List of paragraphs doesn't have 8 paragraphs",
        8, paragraphs.size());
  }

  /**
   * Test on a big page from English wikipedia.
   */
  @Test
  public void testBigPageEn() {

    // Create contents and analysis
    PageAnalysis analysis = analyzeAndTestPage("PageAnalysisTest_en_2020_in_science");

    // Check comments
    ContainerComment commentContainer = analysis.comments();
    assertEquals(
        "List of comments doesn't have 5 comments",
        5, commentContainer.getAll().size());

    // Check tags
    List<PageElementTag> tags = analysis.getTags();
    assertEquals(
        "List of tags doesn't have 1958 tags",
        1958, tags.size());

    // Check internal links
    List<PageElementInternalLink> internalLinks = analysis.getInternalLinks();
    assertEquals(
        "List of internal links doesn't have 1899 internal links",
        1899, internalLinks.size());

    // Check images
    List<PageElementImage> images = analysis.getImages();
    assertEquals(
        "List of images doesn't have 53 images",
        53, images.size());

    // Check categories
    List<PageElementCategory> categories = analysis.getCategories();
    assertEquals(
        "List of categories doesn't have 6 categories",
        6, categories.size());

    // Check interwiki links
    List<PageElementInterwikiLink> iwLinks = analysis.getInterwikiLinks();
    assertEquals(
        "List of interwiki links doesn't have 0 interwiki links",
        0, iwLinks.size());

    // Check language links
    List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
    assertEquals(
        "List of language links doesn't have 0 language links",
        0, langLinks.size());

    // Check functions
    List<PageElementFunction> functions = analysis.getFunctions();
    assertEquals(
        "List of functions doesn't have 0 functions",
        0, functions.size());

    // Check magic words
    List<PageElementMagicWord> magicWords = analysis.getMagicWords();
    assertEquals(
        "List of magic words doesn't have 0 magic words",
        0, magicWords.size());

    // Check templates
    List<PageElementTemplate> templates = analysis.getTemplates();
    assertEquals(
        "List of templates doesn't have 0 templates",
        1013, templates.size());

    // Check parameters
    List<PageElementParameter> parameters = analysis.getParameters();
    assertEquals(
        "List of parameters doesn't have 0 parameters",
        0, parameters.size());

    // Check titles
    List<PageElementTitle> titles = analysis.getTitles();
    assertEquals(
        "List of titles doesn't have 0 titles",
        15, titles.size());

    // Check external links
    List<PageElementExternalLink> eLinks = analysis.getExternalLinks();
    assertEquals(
        "List of external links doesn't have 0 external links",
        702, eLinks.size());

    // Check ISBN
    List<PageElementISBN> isbns = analysis.getISBNs();
    assertEquals(
        "List of ISBN doesn't have 0 ISBN",
        0, isbns.size());

    // Check ISSN
    List<PageElementISSN> issns = analysis.getISSNs();
    assertEquals(
        "List of ISSN doesn't have 0 ISSN",
        76, issns.size());

    // Check PMID
    List<PageElementPMID> pmids = analysis.getPMIDs();
    assertEquals(
        "List of PMID doesn't have 0 PMID",
        106, pmids.size());

    // Check RFC
    List<PageElementRFC> rfcs = analysis.getRFCs();
    assertEquals(
        "List of RFC doesn't have 0 RFC",
        0, rfcs.size());

    // Check tables
    List<PageElementTable> tables = analysis.getTables();
    assertEquals(
        "List of tables doesn't have 0 tables",
        0, tables.size());

    // Check list items
    List<PageElementListItem> listItems = analysis.getListItems();
    assertEquals(
        "List of list items doesn't have 0 list items",
        779, listItems.size());

    // Check paragraphs
    List<PageElementParagraph> paragraphs = analysis.getParagraphs();
    assertEquals(
        "List of paragraphs doesn't have 0 paragraphs",
        51, paragraphs.size());
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
    long beginTime = System.nanoTime();

    // Check comments
    ContainerComment commentContainer = analysis.comments();
    assertNotNull(
        "Comments container is null",
        commentContainer);
    assertNotNull(
        "List of comments is null",
        commentContainer.getAll());

    // Check tags
    List<PageElementTag> tags = analysis.getTags();
    assertNotNull(
        "List of tags is null",
        tags);

    // Check internal links
    List<PageElementInternalLink> internalLinks = analysis.getInternalLinks();
    assertNotNull(
        "List of internal links is null",
        internalLinks);

    // Check images
    List<PageElementImage> images = analysis.getImages();
    assertNotNull(
        "List of images is null",
        images);

    // Check categories
    List<PageElementCategory> categories = analysis.getCategories();
    assertNotNull(
        "List of categories is null",
        categories);

    // Check interwiki links
    List<PageElementInterwikiLink> iwLinks = analysis.getInterwikiLinks();
    assertNotNull(
        "List of interwiki links is null",
        iwLinks);

    // Check language links
    List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
    assertNotNull(
        "List of language links is null",
        langLinks);

    // Check functions
    List<PageElementFunction> functions = analysis.getFunctions();
    assertNotNull(
        "List of functions is null",
        functions);

    // Check magic words
    List<PageElementMagicWord> magicWords = analysis.getMagicWords();
    assertNotNull(
        "List of magic words is null",
        magicWords);

    // Check templates
    List<PageElementTemplate> templates = analysis.getTemplates();
    assertNotNull(
        "List of templates is null",
        templates);

    // Check parameters
    List<PageElementParameter> parameters = analysis.getParameters();
    assertNotNull(
        "List of parameters is null",
        parameters);

    // Check titles
    List<PageElementTitle> titles = analysis.getTitles();
    assertNotNull(
        "List of titles is null",
        titles);

    // Check external links
    List<PageElementExternalLink> eLinks = analysis.getExternalLinks();
    assertNotNull(
        "List of external links is null",
        eLinks);

    // Check ISBN
    List<PageElementISBN> isbns = analysis.getISBNs();
    assertNotNull(
        "List of ISBN is null",
        isbns);

    // Check ISSN
    List<PageElementISSN> issns = analysis.getISSNs();
    assertNotNull(
        "List of ISSN is null",
        issns);

    // Check PMID
    List<PageElementPMID> pmids = analysis.getPMIDs();
    assertNotNull(
        "List of PMID is null",
        pmids);

    // Check RFC
    List<PageElementRFC> rfcs = analysis.getRFCs();
    assertNotNull(
        "List of RFC is null",
        rfcs);

    // Check tables
    List<PageElementTable> tables = analysis.getTables();
    assertNotNull(
        "List of tables is null",
        tables);

    // Check list items
    List<PageElementListItem> listItems = analysis.getListItems();
    assertNotNull(
        "List of list items is null",
        listItems);

    // Check paragraphs
    List<PageElementParagraph> paragraphs = analysis.getParagraphs();
    assertNotNull(
        "List of paragraphs is null",
        paragraphs);

    // Display performance
    long endTime = System.nanoTime();
    System.out.println(fileName + ": " + perf.toMilliSeconds() + "\n  Reading: " + ((endTime - beginTime) / 1000000));

    return analysis;
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
