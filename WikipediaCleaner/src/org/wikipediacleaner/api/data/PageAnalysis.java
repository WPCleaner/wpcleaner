/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.constants.wiki.AbstractWikiSettings;
import org.wikipediacleaner.api.data.contents.Contents;
import org.wikipediacleaner.api.data.contents.ContentsComment;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.api.data.contents.ContentsElementComparator;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.Performance;


/**
 * An analysis of a page.
 */
public class PageAnalysis {

  /** Flag for tracing time taken by execution */
  private static boolean traceTime = false;

  /** Threshold for tracing time taken by execution */
  private final static long TRACE_THRESHOLD = 100;

  /** Page currently analyzed */
  private final Page page;

  /** Current version of the text */
  private final Contents contents;

  /** True if spelling should be checked */
  private boolean checkSpelling;

  /**
   * @param page Page.
   * @param contents Page contents (may differ from page.getContents()).
   */
  PageAnalysis(Page page, String contents) {
    this.page = page;
    this.contents = Contents.createContents((contents != null) ? contents : page.getContents());
    this.areas = new PageElementAreas();

    // Default configuration
    Configuration config = Configuration.getConfiguration();
    checkSpelling = config.getBoolean(
        null, ConfigurationValueBoolean.SPELLING);
  }

  /**
   * @param trace True to force tracing time spent in analysis.
   */
  public static void setTraceTime(boolean trace) {
    traceTime = trace;
  }

  /**
   * @return Page.
   */
  public Page getPage() {
    return page;
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    if (page != null) {
      return page.getWikipedia();
    }
    return null;
  }

  /**
   * @return Wiki settings.
   */
  public AbstractWikiSettings getSettings() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      return wikipedia.getSettings();
    }
    return null;
  }

  /**
   * @return Wiki configuration.
   */
  public WikiConfiguration getWikiConfiguration() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      return wikipedia.getWikiConfiguration();
    }
    return null;
  }

  /**
   * @return WPCleaner configuration.
   */
  public WPCConfiguration getWPCConfiguration() {
    EnumWikipedia wikipedia = getWikipedia();
    if (wikipedia != null) {
      return wikipedia.getConfiguration();
    }
    return null;
  }

  /**
   * @param namespace Namespace.
   * @return true if the page is in the namespace.
   */
  public boolean isInNamespace(int namespace) {
    if ((page != null) &&
        (page.getNamespace() != null)) {
      return (page.getNamespace().intValue() == namespace);
    }
    return false;
  }

  /**
   * @return Page contents.
   */
  public String getContents() {
    return contents.getText();
  }

  /**
   * @param check True if spelling should be checked.
   */
  public void shouldCheckSpelling(boolean check) {
    this.checkSpelling = check;
  }

  /**
   * @return True if spelling should be checked.
   */
  public boolean shouldCheckSpelling() {
    return checkSpelling;
  }

  /**
   * Perform page analysis.
   * 
   * @param perf Performance analysis.
   */
  public void performFullPageAnalysis(AnalysisPerformance perf) {
    long time0 = System.nanoTime();
    level1Analysis();
    long time1 = System.nanoTime();
    level2Analysis();
    long time2 = System.nanoTime();
    level3Analysis();
    long time3 = System.nanoTime();
    level4Analysis();
    long time4 = System.nanoTime();
    level5AnalysisISBN();
    long time4a = System.nanoTime();
    level5AnalysisISSN();
    long time4b = System.nanoTime();
    level5AnalysisPMID();
    long time4c = System.nanoTime();
    level5AnalysisRFC();
    long time5 = System.nanoTime();
    level6Analysis();
    long time6 = System.nanoTime();
    if (perf != null) {
      perf.level1 += (time1 - time0);
      perf.level2 += (time2 - time1);
      perf.level3 += (time3 - time2);
      perf.level4 += (time4 - time3);
      perf.level5 += (time5 - time4);
      perf.level5_ISBN += (time4a - time4);
      perf.level5_ISSN += (time4b - time4a);
      perf.level5_PMID += (time4c - time4b);
      perf.level5_RFC += (time5 - time4c);
      perf.level6 += (time6 - time5);
    }
  }

  /**
   * Bean for holding information about analysis performance.
   */
  public static class AnalysisPerformance {
    long level1;
    long level2;
    long level3;
    long level4;
    long level5;
    long level5_ISBN;
    long level5_ISSN;
    long level5_PMID;
    long level5_RFC;
    long level6;

    public AnalysisPerformance() {
      level1 = 0;
      level2 = 0;
      level3 = 0;
      level4 = 0;
      level5 = 0;
      level5_ISBN = 0;
      level5_ISSN = 0;
      level5_PMID = 0;
      level5_RFC = 0;
      level6 = 0;
    }

    /**
     * @return Textual description of the object.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      long time1 = level1 / 1000000000;
      long time2 = level2 / 1000000000;
      long time3 = level3 / 1000000000;
      long time4 = level4 / 1000000000;
      long time5 = level5 / 1000000000;
      long time5_ISBN = level5_ISBN / 1000000000;
      long time5_ISSN = level5_ISSN / 1000000000;
      long time5_PMID = level5_PMID / 1000000000;
      long time5_RFC = level5_RFC / 1000000000;
      long time6 = level6 / 1000000000;
      StringBuilder result = new StringBuilder();
      result.append(time1 + time2 + time3 + time4 + time5 + time6);
      result.append(" s (");
      result.append(time1);
      result.append(" + ");
      result.append(time2);
      result.append(" + ");
      result.append(time3);
      result.append(" + ");
      result.append(time4);
      result.append(" + ");
      result.append(time5);
      if (time5_ISBN + time5_ISSN + time5_PMID + time5_RFC > 0) {
        result.append("=");
        result.append(time5_ISBN);
        result.append("+");
        result.append(time5_ISSN);
        result.append("+");
        result.append(time5_PMID);
        result.append("+");
        result.append(time5_RFC);
      }
      result.append(" + ");
      result.append(time6);
      result.append(")");
      return result.toString();
    }

  }

  // ==========================================================================
  // Elements management
  // ==========================================================================

  /**
   * Management of non wiki text areas.
   */
  private final PageElementAreas areas;

  /**
   * @return List of non wiki text areas.
   */
  public PageElementAreas getAreas() {
    level6Analysis();
    return areas;
  }

  /**
   * @param withCategories True if categories should be included in the result.
   * @param withComments True if comments should be included in the result.
   * @param withExternalLinks True if external links should be included in the result.
   * @param withFunctions True if functions should be included in the result.
   * @param withImages True if images should be included in the result.
   * @param withInternalLinks True if internal links should be included in the result.
   * @param withInterwikiLinks True if interwiki links should be included in the result.
   * @param withLanguageLinks True if language links should be included in the result.
   * @param withListItems True if list items should be included in the result.
   * @param withMagicWords True if magic words should be included in the result.
   * @param withParameters True if parameters should be included in the result.
   * @param withTables True if tables should be included in the result.
   * @param withTags True if tags should be included in the result.
   * @param withTemplates True if templates should be included in the result.
   * @param withTitles True if titles should be included in the result.
   * @return All elements.
   */
  public List<ContentsElement> getElements(
      boolean withCategories, boolean withComments,
      boolean withExternalLinks, boolean withFunctions,
      boolean withImages, boolean withInternalLinks,
      boolean withInterwikiLinks, boolean withLanguageLinks,
      boolean withListItems,
      boolean withMagicWords, boolean withParameters,
      boolean withTables, boolean withTags,
      boolean withTemplates, boolean withTitles) {
    List<ContentsElement> elements = new ArrayList<>();
    if (withCategories) {
      elements.addAll(getCategories());
    }
    if (withComments) {
      elements.addAll(getComments());
    }
    if (withExternalLinks) {
      elements.addAll(getExternalLinks());
    }
    if (withFunctions) {
      elements.addAll(getFunctions());
    }
    if (withImages) {
      elements.addAll(getImages());
    }
    if (withInternalLinks) {
      elements.addAll(getInternalLinks());
    }
    if (withInterwikiLinks) {
      elements.addAll(getInterwikiLinks());
    }
    if (withLanguageLinks) {
      elements.addAll(getLanguageLinks());
    }
    if (withListItems) {
      elements.addAll(getListItems());
    }
    if (withMagicWords) {
      elements.addAll(getMagicWords());
    }
    if (withParameters) {
      elements.addAll(getParameters());
    }
    if (withTables) {
      elements.addAll(getTables());
    }
    if (withTags) {
      elements.addAll(getTags());
    }
    if (withTemplates) {
      elements.addAll(getTemplates());
    }
    if (withTitles) {
      elements.addAll(getTitles());
    }
    Collections.sort(elements, new ContentsElementComparator());
    return elements;
  }

  /**
   * @param currentIndex Index.
   * @return Element at the specified index.
   */
  public ContentsElement isInElement(int currentIndex) {

    // Check if in comment
    ContentsElement element = isInComment(currentIndex);
    if (element != null) {
      return element;
    }

    // Check if in internal link
    PageElementInternalLink internalLink = isInInternalLink(currentIndex);
    element = internalLink;

    // Check if in template
    PageElementTemplate template = isInTemplate(currentIndex);
    if ((template != null) &&
        ((element == null) || (element.getBeginIndex() < template.getBeginIndex()))) {
      element = template;
    }

    // Check if in image
    PageElementImage image = isInImage(currentIndex);
    if ((image != null) &&
        ((element == null) || (element.getBeginIndex() < image.getBeginIndex()))) {
      element = image;
    }

    // Check if in category
    PageElementCategory category = isInCategory(currentIndex);
    if ((category != null) &&
        ((element == null) || (element.getBeginIndex() < category.getBeginIndex()))) {
      element = category;
    }

    // Check if in interwiki
    PageElementInterwikiLink interwiki = isInInterwikiLink(currentIndex);
    if ((interwiki != null) &&
        ((element == null) || (element.getBeginIndex() < interwiki.getBeginIndex()))) {
      element = interwiki;
    }

    // Check if in language link
    PageElementLanguageLink language = isInLanguageLink(currentIndex);
    if ((language != null) &&
        ((element == null) || (element.getBeginIndex() < language.getBeginIndex()))) {
      element = language;
    }

    // Check if in external link
    PageElementExternalLink externalLink = isInExternalLink(currentIndex);
    if ((externalLink != null) &&
        ((element == null) || (element.getBeginIndex() < externalLink.getBeginIndex()))) {
      element = externalLink;
    }

    // Check if in tag
    PageElementTag tag = isInTag(currentIndex);
    if ((tag != null) &&
        ((element == null) || (element.getBeginIndex() < tag.getBeginIndex()))) {
      element = tag;
    }

    // Check if in parameter
    PageElementParameter parameter = isInParameter(currentIndex);
    if ((parameter != null) &&
        ((element == null) || (element.getBeginIndex() < parameter.getBeginIndex()))) {
      element = parameter;
    }

    // Check if in function
    PageElementFunction function = isInFunction(currentIndex);
    if ((function != null) &&
        ((element == null) || (element.getBeginIndex() < function.getBeginIndex()))) {
      element = function;
    }

    // Check if in ISBN
    PageElementISBN isbn = isInISBN(currentIndex);
    if ((isbn != null) &&
        ((element == null) || (element.getBeginIndex() < isbn.getBeginIndex()))) {
      element = isbn;
    }

    // Check if in table
    PageElementTable table = isInTable(currentIndex);
    if ((table != null) &&
        ((element == null) || (element.getBeginIndex() < table.getBeginIndex()))) {
      element = table;
    }

    return element;
  }

  /**
   * Check if two indexes are in the same area.
   * 
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @return True if begin and end indexes are in the same area.
   */
  public boolean areInSameArea(int beginIndex, int endIndex) {

    // Check for internal links
    PageElementInternalLink iLink = isInInternalLink(beginIndex);
    if ((iLink != null) && (iLink.getBeginIndex() < beginIndex)) {
      if (iLink.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInInternalLink(endIndex) != null) {
        return false;
      }
    }

    // Check for external links
    PageElementExternalLink eLink = isInExternalLink(beginIndex);
    if ((eLink != null) && (eLink.getBeginIndex() < beginIndex)) {
      if (eLink.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInExternalLink(endIndex) != null) {
        return false;
      }
    }

    // Check for templates
    PageElementTemplate template = isInTemplate(beginIndex);
    if ((template != null) && (template.getBeginIndex() < beginIndex)) {
      if (template.getEndIndex() < endIndex) {
        return false;
      }
      PageElementTemplate.Parameter param = template.getParameterAtIndex(beginIndex);
      if ((param != null) && (param.getEndIndex() < endIndex)) {
        return false;
      }
    } else {
      if (isInTemplate(endIndex) != null) {
        return false;
      }
    }

    // Check for images
    PageElementImage image = isInImage(beginIndex);
    if ((image != null) && (image.getBeginIndex() < beginIndex)) {
      if (image.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInImage(endIndex) != null) {
        return false;
      }
    }

    // Check for parameters
    PageElementParameter parameter = isInParameter(beginIndex);
    if ((parameter != null) && (parameter.getBeginIndex() < beginIndex)) {
      if (parameter.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInParameter(endIndex) != null) {
        return false;
      }
    }

    // Check for functions
    PageElementFunction function = isInFunction(beginIndex);
    if ((function != null) && (function.getBeginIndex() < beginIndex)) {
      if (function.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInFunction(endIndex) != null) {
        return false;
      }
    }

    // Check for magic words
    PageElementMagicWord magicWord = isInMagicWord(beginIndex);
    if ((magicWord != null) && (magicWord.getBeginIndex() < beginIndex)) {
      if (magicWord.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInMagicWord(endIndex) != null) {
        return false;
      }
    }

    // Check for tags
    List<PageElementTag> allTags = getTags();
    for (PageElementTag tag : allTags) {
      if (!tag.isFullTag()) {
        if (tag.isComplete()) {
          int tagBeginIndex = tag.getCompleteBeginIndex();
          int tagEndIndex = tag.getCompleteEndIndex();
          if ((beginIndex > tagBeginIndex) && (beginIndex < tagEndIndex)) {
            if (endIndex >= tagEndIndex) {
              return false;
            }
          } else if ((endIndex > tagBeginIndex) && (endIndex < tagEndIndex)) {
            return false;
          }
        } else {
          // NOTE: should we check for other unclosed tags?
        }
      }
    }

    // Check for tables
    PageElementTable table = isInTable(beginIndex);
    if (table != null) {
      if (table.getEndIndex() < endIndex) {
        return false;
      }
      PageElementTable.TableCell cell = table.getCellAtIndex(beginIndex);
      if ((cell != null) && (cell.getEndIndex() < endIndex)) {
        return false;
      }
    } else {
      if (isInTable(endIndex) != null) {
        return false;
      }
    }

    // Check for lists
    PageElementListItem listItem = isInListItem(beginIndex);
    if (listItem != null) {
      if (listItem.getEndIndex() < endIndex) {
        return false;
      }
    } else {
      if (isInListItem(endIndex) != null) {
        return false;
      }
    }
    return true;
  }

  // ==========================================================================
  // Content analysis
  // ==========================================================================

  /** Internal lock for level 1 analysis. */
  private final Object level1Lock = new Object();
  private boolean level1Done = false;

  /** Internal lock for level 2 analysis. */
  private final Object level2Lock = new Object();

  /** Internal lock for level 3 analysis. */
  private final Object level3Lock = new Object();

  /** Internal lock for level 4 analysis. */
  private final Object level4Lock = new Object();

  /** Internal lock for level 5 analysis. */
  private final Object level5Lock = new Object();

  /** Internal lock for level 6 analysis. */
  private final Object level6Lock = new Object();

  /**
   * Perform a level 1 analysis of the page (comments).
   */
  private void level1Analysis() {
    synchronized (level1Lock) {
      if (level1Done) {
        return;
      }

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level1Analysis", TRACE_THRESHOLD);
        perf.printStart();
      }

      // Update areas of non wiki text
      areas.addComments(getComments());

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level analysis 2 of the page (tags).
   */
  private void level2Analysis() {
    synchronized (level2Lock) {
      if (tags != null) {
        return;
      }
      level1Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level2Analysis", TRACE_THRESHOLD);
        perf.printStart();
      }

      // Initialize
      tags = new ArrayList<PageElementTag>();

      // Go through all the text of the page
      int maxIndex = contents.length();
      String text = contents.getText();
      int currentIndex = 0;
      while (currentIndex < maxIndex) {
        currentIndex = text.indexOf('<', currentIndex);
        if (currentIndex < 0) {
          currentIndex = maxIndex;
        } else {
          int nextIndex = areas.getEndArea(currentIndex);
          if (nextIndex > currentIndex) {
            currentIndex = nextIndex;
          } else {
            PageElementTag tag = PageElementTag.analyzeBlock(text, currentIndex);
            if (tag != null) {
              if (tag.isEndTag() && !tag.isFullTag()) {
                boolean found = false;
                int i = tags.size();
                int level = 0;
                while ((i > 0) && !found) {
                  i--;
                  PageElementTag tmpTag = tags.get(i);
                  if (tag.getNormalizedName().equals(tmpTag.getNormalizedName())) {
                    if (!tmpTag.isFullTag()) {
                      if (tmpTag.isEndTag()) {
                        level++;
                      } else {
                        level--;
                        if (level < 0) {
                          found = true;
                          tmpTag.setMatchingTag(tag);
                        }
                      }
                    }
                  }
                }
              }
              tags.add(tag);
              currentIndex = tag.getEndIndex();
            } else {
              currentIndex++;
            }
          }
        }
      }

      // Update areas of non wiki text
      areas.addTags(tags);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 3 analysis of the page (links, templates, ...).
   */
  private void level3Analysis() {
    synchronized (level3Lock) {
      if (internalLinks != null) {
        return;
      }
      level2Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level3Analysis", TRACE_THRESHOLD);
        perf.printStart();
        perf.startPart();
      }

      internalLinks = new ArrayList<PageElementInternalLink>();
      images = new ArrayList<PageElementImage>();
      categories = new ArrayList<PageElementCategory>();
      interwikiLinks = new ArrayList<PageElementInterwikiLink>();
      languageLinks = new ArrayList<PageElementLanguageLink>();
      functions = new ArrayList<PageElementFunction>();
      magicWords = new ArrayList<PageElementMagicWord>();
      templates = new ArrayList<PageElementTemplate>();
      parameters = new ArrayList<PageElementParameter>();
      titles = new ArrayList<PageElementTitle>();
      if (perf != null) {
        perf.stopPart("new");
      }

      // Go through all the text of the page
      int maxIndex = contents.length();
      String text = contents.getText();
      int currentIndex = 0;
      int areaIndex = 0;
      List<PageElementAreas.Area> tmpAeras = areas.getAreas();
      while (currentIndex < maxIndex) {

        // Checking if the current index is in wiki text area.
        boolean areaFound = false;
        int nextIndex = currentIndex;
        while ((areaIndex < tmpAeras.size()) && !areaFound) {
          PageElementAreas.Area area = tmpAeras.get(areaIndex);
          if (area.beginIndex > currentIndex) {
            areaFound = true;
          } else if (area.endIndex > currentIndex) {
            areaFound = true;
            nextIndex = area.endIndex;
          } else {
            areaIndex++;
          }
        }
        if (perf != null) {
          perf.stopPart("nextIndex");
        }

        if (nextIndex > currentIndex) {
          currentIndex = nextIndex;
        } else {
          if (text.startsWith("[[", currentIndex)) {
            currentIndex = analyze2SquareBrackets(currentIndex);
            if (perf != null) {
              perf.stopPart("analyze2SquareBrackets");
            }
          } else if (text.startsWith("{{{", currentIndex)) {
            currentIndex = analyze3CurlyBrackets(currentIndex);
            if (perf != null) {
              perf.stopPart("analyze3CurlyBrackets");
            }
          } else if (text.startsWith("{{", currentIndex)) {
            currentIndex = analyze2CurlyBrackets(currentIndex);
            if (perf != null) {
              perf.stopPart("analyze2CurlyBrackets");
            }
          } else if (text.startsWith("=", currentIndex)) {
            currentIndex = analyze1Equal(currentIndex);
            if (perf != null) {
              perf.stopPart("analyze1Equal");
            }
          } else if (text.startsWith("__", currentIndex)) {
            currentIndex = analyze2Undescore(currentIndex);
            if (perf != null) {
              perf.stopPart("analyze2UnderscoreBrackets");
            }
          } else {
            currentIndex++;
          }
        }
      }

      // Update areas of non wiki text
      areas.addInternalLinks(internalLinks);
      areas.addImages(images);
      areas.addCategories(categories);
      areas.addInterwikiLinks(interwikiLinks);
      areas.addLanguageLinks(languageLinks);
      areas.addTemplates(templates);
      areas.addFunctions(functions);
      areas.addMagicWords(magicWords);
      areas.addParameters(parameters);
      areas.addTitles(titles);

      if (perf != null) {
        perf.stopPart("addAreas");
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 4 analysis of the page (external links).
   */
  private void level4Analysis() {
    synchronized (level4Lock) {
      if (externalLinks != null) {
        return;
      }
      level3Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level4Analysis", TRACE_THRESHOLD);
        perf.printStart();
      }

      // Go through all the text of the page
      externalLinks = new ArrayList<PageElementExternalLink>();
      int maxIndex = contents.length();
      String text = contents.getText();
      int currentIndex = 0;
      int areaIndex = 0;
      List<PageElementAreas.Area> tmpAeras = areas.getAreas();
      while (currentIndex < maxIndex) {

        // Checking if the current index is in wiki text area.
        boolean areaFound = false;
        int nextIndex = currentIndex;
        while ((areaIndex < tmpAeras.size()) && !areaFound) {
          PageElementAreas.Area area = tmpAeras.get(areaIndex);
          if (area.beginIndex > currentIndex) {
            areaFound = true;
          } else if (area.endIndex > currentIndex) {
            areaFound = true;
            nextIndex = area.endIndex;
          } else {
            areaIndex++;
          }
        }

        if (nextIndex > currentIndex) {
          currentIndex = nextIndex;
        } else {
          if (text.startsWith("[", currentIndex)) {
            currentIndex = analyze1SquareBracket(currentIndex);
            if (perf != null) {
              perf.stopPart("analyze1SquareBracket");
            }
          } else {
            currentIndex = analyzeText(currentIndex);
            if (perf != null) {
              perf.stopPart("analyzeText");
            }
          }
        }
      }
      areas.addExternalLinks(externalLinks);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 5 analysis of the page (identifiers).
   */
  private void level5Analysis() {
    synchronized (level5Lock) {
      if ((isbns != null) || (issns != null) || (pmids != null)) {
        return;
      }
      level4Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level5Analysis", TRACE_THRESHOLD);
        perf.printStart();
      }

      isbns = PageElementISBN.analyzePage(this);
      areas.addISBN(isbns);
      issns = PageElementISSN.analyzePage(this);
      areas.addISSN(issns);
      pmids = PageElementPMID.analyzePage(this);
      areas.addPMID(pmids);
      rfcs = PageElementRFC.analyzePage(this);
      areas.addRFC(rfcs);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 5 analysis of the page for ISBN.
   */
  private void level5AnalysisISBN() {
    synchronized (level5Lock) {
      if (isbns != null) {
        return;
      }
      level4Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level5AnalysisISBN", TRACE_THRESHOLD);
        perf.printStart();
      }

      isbns = PageElementISBN.analyzePage(this);
      areas.addISBN(isbns);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 5 analysis of the page for ISSN.
   */
  private void level5AnalysisISSN() {
    synchronized (level5Lock) {
      if (issns != null) {
        return;
      }
      level4Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level5AnalysisISSN", TRACE_THRESHOLD);
        perf.printStart();
      }

      issns = PageElementISSN.analyzePage(this);
      areas.addISSN(issns);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 5 analysis of the page for PMID.
   */
  private void level5AnalysisPMID() {
    synchronized (level5Lock) {
      if (pmids != null) {
        return;
      }
      level4Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level5AnalysisPMID", TRACE_THRESHOLD);
        perf.printStart();
      }

      pmids = PageElementPMID.analyzePage(this);
      areas.addPMID(pmids);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 5 analysis of the page for RFC.
   */
  private void level5AnalysisRFC() {
    synchronized (level5Lock) {
      if (rfcs != null) {
        return;
      }
      level4Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level5AnalysisRFC", TRACE_THRESHOLD);
        perf.printStart();
      }

      rfcs = PageElementRFC.analyzePage(this);
      areas.addRFC(rfcs);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Perform a level 6 analysis of the page (tables).
   */
  private void level6Analysis() {
    synchronized (level6Lock) {
      if ((tables != null) || (listItems != null)) {
        return;
      }
      level5Analysis();

      Performance perf = null;
      if (traceTime) {
        perf = Performance.getInstance(
            "PageAnalysis.level6Analysis", TRACE_THRESHOLD);
        perf.printStart();
      }

      tables = PageElementTable.analyzePage(this);
      // TODO: areas.addTables(tables);
      listItems = PageElementListItem.analyzePage(this);
      // TODO: areas.addListItems(listItems);
      paragraphs = PageElementParagraph.analyzePage(this);
      // TODO: areas.addParagraph(paragraphs);

      if (perf != null) {
        perf.printEndAlways();
        perf.release();
      }
    }
  }

  /**
   * Part of the analysis when text is beginning with "[[".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze2SquareBrackets(int currentIndex) {

    String text = contents.getText();

    // Check if this is an internal link
    PageElementInternalLink link = PageElementInternalLink.analyzeBlock(
        getWikipedia(), text, currentIndex);
    if (link != null) {
      internalLinks.add(link);
      if (link.getText() == null) {
        return link.getEndIndex();
      }
      return link.getBeginIndex() + Math.max(2, link.getTextOffset());
    }

    // Check if this is an image
    PageElementImage image = PageElementImage.analyzeBlock(
        getWikipedia(), text, currentIndex);
    if (image != null) {
      images.add(image);
      return image.getBeginIndex() + 2 + image.getNamespace().length() + 1;
    }

    // Check if this is a category
    PageElementCategory category = PageElementCategory.analyzeBlock(
        getWikipedia(), text, currentIndex);
    if (category != null) {
      categories.add(category);
      return category.getEndIndex();
    }

    // Check if this is an interwiki link
    PageElementInterwikiLink interwiki = PageElementInterwikiLink.analyzeBlock(
        getWikipedia(), text, currentIndex);
    if (interwiki != null) {
      interwikiLinks.add(interwiki);
      if (interwiki.getText() == null) {
        return interwiki.getEndIndex();
      }
      return interwiki.getBeginIndex() + Math.max(2, interwiki.getTextOffset());
    }

    // Check if this is a language link
    PageElementLanguageLink language = PageElementLanguageLink.analyzeBlock(
        getWikipedia(), text, currentIndex);
    if (language != null) {
      languageLinks.add(language);
      return language.getEndIndex();
    }

    return currentIndex + 1;
  }

  /**
   * Part of the analysis when text is beginning with "[".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze1SquareBracket(int currentIndex) {

    // Check if this an external link
    String text = contents.getText();
    PageElementExternalLink link = PageElementExternalLink.analyzeBlock(
        getWikipedia(), text, currentIndex, this);
    if (link != null) {
      externalLinks.add(link);
      if (link.getText() == null) {
        return link.getEndIndex();
      }
      return link.getBeginIndex() + Math.max(2, link.getTextOffset());
    }

    return currentIndex + 1;
  }

  /**
   * Part of the analysis when text is beginning with "__".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze2Undescore(int currentIndex) {

    // Check if this a magic word
    String text = contents.getText();
    PageElementMagicWord magicWord = PageElementMagicWord.analyzeBlock(
        getWikipedia(), text, currentIndex);
    if (magicWord != null) {
      magicWords.add(magicWord);
      return magicWord.getEndIndex();
    }

    return currentIndex + 1;
  }

  /**
   * Part of the analysis for regular text.
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyzeText(int currentIndex) {

    // Check if this is an external link
    String text = contents.getText();
    if ((externalLinks.size() == 0) ||
        (externalLinks.get(externalLinks.size() - 1).getEndIndex() <= currentIndex)) {
      PageElementExternalLink link = PageElementExternalLink.analyzeBlock(
          getWikipedia(), text, currentIndex, this);
      if (link != null) {
        externalLinks.add(link);
        return link.getEndIndex();
      }
    }

    return currentIndex + 1;
  }

  /**
   * Part of the analysis when text is beginning with "{{{".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze3CurlyBrackets(int currentIndex) {

    // Check if this is a parameter
    String text = contents.getText();
    PageElementParameter parameter = PageElementParameter.analyzeBlock(
        getWikipedia(), text, currentIndex, getComments(), tags);
    if (parameter != null) {
      parameters.add(parameter);
      return currentIndex + 3;
    }

    return currentIndex + 1;
  }

  /**
   * Part of the analysis when text is beginning with "{{".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze2CurlyBrackets(int currentIndex) {

    String text = contents.getText();

    // Check if this is a function
    PageElementFunction function = PageElementFunction.analyzeBlock(
        getWikipedia(), text, currentIndex, getComments(), tags);
    if (function != null) {
      functions.add(function);
      if (function.getParameterCount() == 0) {
        return function.getEndIndex();
      }
      return currentIndex + 2;
    }

    // Check if this is a template
    PageElementTemplate template = PageElementTemplate.analyzeBlock(
        getWikipedia(), text, currentIndex, getComments(), tags);
    if (template != null) {
      templates.add(template);
      if (template.getParameterCount() == 0) {
        return template.getEndIndex();
      }
      return currentIndex + 2;
    }

    return currentIndex + 1;
  }

  /**
   * Part of the analysis when text is beginning with "=".
   * 
   * @param currentIndex Current index in the text.
   * @return Next index.
   */
  private int analyze1Equal(int currentIndex) {

    // Check that it's a beginning of a line
    String text = contents.getText();
    boolean hasNewLine = false;
    int tmpIndex = currentIndex;
    while ((tmpIndex >= 0) && !hasNewLine) {
      tmpIndex--;
      if (tmpIndex < 0) {
        hasNewLine = true;
      } else if (text.charAt(tmpIndex) == '\n') {
        hasNewLine = true;
      } else if (text.charAt(tmpIndex) == '>') {
        ContentsComment comment = null;
        for (ContentsComment tmpComment : getComments()) {
          if (tmpComment.getEndIndex() == tmpIndex + 1) {
            comment = tmpComment;
          }
        }
        if (comment == null) {
          return currentIndex + 1;
        }
        tmpIndex = comment.getBeginIndex();
      } else {
        return currentIndex + 1;
      }
    }

    // Check that it's not a template value
    if (templates != null) {
      PageElementTemplate template = null;
      for (PageElementTemplate tmp : templates) {
        if ((tmp.getBeginIndex() <= currentIndex) &&
            (tmp.getEndIndex() > currentIndex)) {
          template = tmp;
        }
      }
      if (template != null) {
        for (int i = 0; i < template.getParameterCount(); i++) {
          int beginParam = template.getParameterPipeIndex(i);
          int endParam = template.getParameterValueStartIndex(i);
          if ((currentIndex >= beginParam) && (currentIndex < endParam)) {
            return currentIndex + 1;
          }
        }
      }
    }

    // Check if this is a title
    PageElementTitle title = PageElementTitle.analyzeBlock(
        getWikipedia(), text, currentIndex, getComments(), tags);
    if (title != null) {
      titles.add(title);
      return title.getBeginIndex() + title.getFirstLevel();
    }

    return currentIndex + 1;
  }

  // ==========================================================================
  // Comments management
  // ==========================================================================

  /**
   * @return All comments in the page.
   */
  public List<ContentsComment> getComments() {
    return contents.getComments().getElements();
  }

  /**
   * @param currentIndex Current index.
   * @return Comment if the current index is inside a comment.
   */
  public ContentsComment isInComment(int currentIndex) {
    return contents.getComments().getSmallestElementAtIndex(currentIndex);
  }

  // ==========================================================================
  // Titles management
  // ==========================================================================

  /**
   * All titles in the page.
   */
  private List<PageElementTitle> titles;

  /**
   * @return All titles in the page.
   */
  public List<PageElementTitle> getTitles() {
    level3Analysis();
    return titles;
  }

  /**
   * @param currentIndex Current index.
   * @return Next title.
   */
  public PageElementTitle getNextTitle(int currentIndex) {
    List<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if (title.getBeginIndex() >= currentIndex) {
        return title;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Title if the current index is inside a title.
   */
  public PageElementTitle isInTitle(int currentIndex) {
    List<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if ((title.getBeginIndex() <= currentIndex) &&
          (title.getEndIndex() > currentIndex)) {
        return title;
      }
    }
    return null;
  }

  /**
   * @return True if titles seem to be reliable.
   */
  public boolean areTitlesReliable() {
    List<PageElementTitle> tmpTitles = getTitles();
    for (PageElementTitle title : tmpTitles) {
      if (!title.isCoherent()) {
        return false;
      }
      if (isInTemplate(title.getBeginIndex()) != null) {
        return false;
      }
    }
    return true;
  }

  // ==========================================================================
  // Internal links management
  // ==========================================================================

  /**
   * All internal links in the page.
   */
  private List<PageElementInternalLink> internalLinks;

  /**
   * @return All internal links in the page.
   */
  public List<PageElementInternalLink> getInternalLinks() {
    level3Analysis();
    return internalLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next internal link.
   */
  public PageElementInternalLink getNextInternalLink(int currentIndex) {
    List<PageElementInternalLink> tmpInternalLinks = getInternalLinks();
    for (PageElementInternalLink link : tmpInternalLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Internal link if the current index is inside an internal link.
   */
  public PageElementInternalLink isInInternalLink(int currentIndex) {
    List<PageElementInternalLink> tmpLinks = getInternalLinks();
    for (PageElementInternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  /**
   * Links count.
   */
  private Map<String, InternalLinkCount> linksCount = new HashMap<String, InternalLinkCount>();

  /**
   * @param link Link.
   * @return Number of links to the page.
   */
  public InternalLinkCount getLinkCount(Page link) {
    InternalLinkCount result = linksCount.get(link.getTitle());
    if (result != null) {
      return result;
    }
    List<Page> links = Collections.singletonList(link);
    InternalLinkCounter counter = new InternalLinkCounter(linksCount, links);
    PageAnalysisUtils.findInternalLinks(this, links, counter);
    return linksCount.get(link.getTitle());
  }

  /**
   * Count number of links in the page.
   * 
   * @param links Links.
   */
  public void countLinks(List<Page> links) {
    if ((links == null) || (links.size() == 0)) {
      return;
    }
    List<Page> interestingLinks = new ArrayList<Page>();
    for (Page link : links) {
      if (!linksCount.containsKey(link.getTitle())) {
        interestingLinks.add(link);
      }
    }
    if (interestingLinks.size() > 0) {
      InternalLinkCounter counter = new InternalLinkCounter(linksCount, interestingLinks);
      PageAnalysisUtils.findInternalLinks(this, interestingLinks, counter);
    }
  }

  // ==========================================================================
  // Images management
  // ==========================================================================

  /**
   * All images in the page.
   */
  private List<PageElementImage> images;

  /**
   * @return All images in the page.
   */
  public List<PageElementImage> getImages() {
    level3Analysis();
    return images;
  }

  /**
   * @param currentIndex Current index.
   * @return Next image.
   */
  public PageElementImage getNextImage(int currentIndex) {
    List<PageElementImage> tmpImages = getImages();
    for (PageElementImage image : tmpImages) {
      if (image.getBeginIndex() >= currentIndex) {
        return image;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Image if the current index is inside an image.
   */
  public PageElementImage isInImage(int currentIndex) {
    List<PageElementImage> tmpImages = getImages();
    PageElementImage result = null;
    for (PageElementImage image : tmpImages) {
      if ((image.getBeginIndex() <= currentIndex) &&
          (image.getEndIndex() > currentIndex)) {
        if ((result == null) ||
            (image.getBeginIndex() > result.getBeginIndex())) {
          result = image;
        }
      }
    }
    return result;
  }

  // ==========================================================================
  // External links management
  // ==========================================================================

  /**
   * All external links in the page.
   */
  private List<PageElementExternalLink> externalLinks;

  /**
   * @return All external links in the page.
   */
  public List<PageElementExternalLink> getExternalLinks() {
    level4Analysis();
    return externalLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next external link.
   */
  public PageElementExternalLink getNextExternalLink(int currentIndex) {
    List<PageElementExternalLink> tmpExternalLinks = getExternalLinks();
    for (PageElementExternalLink link : tmpExternalLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return External link if the current index is inside an external link.
   */
  public PageElementExternalLink isInExternalLink(int currentIndex) {
    List<PageElementExternalLink> tmpLinks = getExternalLinks();
    for (PageElementExternalLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  // ==========================================================================
  // Templates management
  // ==========================================================================

  /**
   * All templates in the page.
   */
  private List<PageElementTemplate> templates;

  /**
   * @return All templates in the page.
   */
  public List<PageElementTemplate> getTemplates() {
    level3Analysis();
    return templates;
  }

  /**
   * @param name Template name.
   * @return All templates with this name in the page analysis.
   */
  public List<PageElementTemplate> getTemplates(String name) {
    if (name == null) {
      return null;
    }
    List<PageElementTemplate> tmpTemplates = getTemplates();
    List<PageElementTemplate> result = new ArrayList<PageElementTemplate>();
    if (tmpTemplates != null) {
      for (PageElementTemplate template : tmpTemplates) {
        if (Page.areSameTitle(name, template.getTemplateName())) {
          result.add(template);
        }
      }
    }
    return result;
  }

  /**
   * @param currentIndex Current index.
   * @return Next template.
   */
  public PageElementTemplate getNextTemplate(int currentIndex) {
    List<PageElementTemplate> tmpTemplates = getTemplates();
    for (PageElementTemplate template : tmpTemplates) {
      if (template.getBeginIndex() >= currentIndex) {
        return template;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Template if the current index is inside a template.
   */
  public PageElementTemplate isInTemplate(int currentIndex) {
    List<PageElementTemplate> tmpTemplates = getTemplates();
    PageElementTemplate result = null;
    for (PageElementTemplate template : tmpTemplates) {
      if ((template.getBeginIndex() <= currentIndex) &&
          (template.getEndIndex() > currentIndex)) {
        result = template;
      }
    }
    return result;
  }

  // ==========================================================================
  // Parameters management
  // ==========================================================================

  /**
   * All parameters in the page.
   */
  private List<PageElementParameter> parameters;

  /**
   * @return All parameters in the page.
   */
  public List<PageElementParameter> getParameters() {
    level3Analysis();
    return parameters;
  }

  /**
   * @param currentIndex Current index.
   * @return Parameter if the current index is inside a parameter.
   */
  public PageElementParameter isInParameter(int currentIndex) {
    List<PageElementParameter> tmpParameters = getParameters();
    PageElementParameter result = null;
    for (PageElementParameter parameter : tmpParameters) {
      if ((parameter.getBeginIndex() <= currentIndex) &&
          (parameter.getEndIndex() > currentIndex)) {
        result = parameter;
      }
    }
    return result;
  }

  // ==========================================================================
  // Functions management
  // ==========================================================================

  /**
   * All functions in the page.
   */
  private List<PageElementFunction> functions;

  /**
   * @return All functions in the page.
   */
  public List<PageElementFunction> getFunctions() {
    level3Analysis();
    return functions;
  }

  /**
   * @param currentIndex Current index.
   * @return Function if the current index is inside a function.
   */
  public PageElementFunction isInFunction(int currentIndex) {
    List<PageElementFunction> tmpFunctions = getFunctions();
    PageElementFunction result = null;
    for (PageElementFunction function : tmpFunctions) {
      if ((function.getBeginIndex() <= currentIndex) &&
          (function.getEndIndex() > currentIndex)) {
        result = function;
      }
    }
    return result;
  }

  // ==========================================================================
  // Magic words management
  // ==========================================================================

  /**
   * All magic words in the page.
   */
  private List<PageElementMagicWord> magicWords;

  /**
   * @return All magic words in the page.
   */
  public List<PageElementMagicWord> getMagicWords() {
    level3Analysis();
    return magicWords;
  }

  /**
   * @param currentIndex Current index.
   * @return Magic word if the current index is inside a magic word.
   */
  public PageElementMagicWord isInMagicWord(int currentIndex) {
    List<PageElementMagicWord> tmpMagicWords = getMagicWords();
    PageElementMagicWord result = null;
    for (PageElementMagicWord magicWord : tmpMagicWords) {
      if ((magicWord.getBeginIndex() <= currentIndex) &&
          (magicWord.getEndIndex() > currentIndex)) {
        result = magicWord;
      }
    }
    return result;
  }

  // ==========================================================================
  // Tags management
  // ==========================================================================

  /**
   * All tags in the page.
   */
  private List<PageElementTag> tags;

  /**
   * Lock for updating the tags categorized by name.
   */
  private final Object lockTagsByName = new Object();

  /**
   * All tags in the page categorized by name.
   */
  private Map<String, List<PageElementTag>> tagsByName;

  /**
   * All complete tags in the page categorized by name.
   * Complete tags are either full tags or opening tags associated with a closing tag.
   */
  private Map<String, List<PageElementTag>> completeTagsByName;

  /**
   * @return All tags in the page.
   */
  public List<PageElementTag> getTags() {
    level2Analysis();
    return tags;
  }

  /**
   * @param name Tag name.
   * @return All tags with this name in the page.
   */
  public List<PageElementTag> getTags(String name) {
    if (name == null) {
      return null;
    }
    synchronized (lockTagsByName) {
      if (tagsByName == null) {
        tagsByName = new HashMap<String, List<PageElementTag>>();
      }
      name = name.toLowerCase();
      List<PageElementTag> result = tagsByName.get(name);
      if (result == null) {
        List<PageElementTag> tmpTags = getTags();
        result = new ArrayList<PageElementTag>();
        tagsByName.put(name, result);
        for (PageElementTag tag : tmpTags) {
          if (name.equals(tag.getNormalizedName())) {
            result.add(tag);
          }
        }
      }
      return result;
    }
  }

  /**
   * @param name Tag name.
   * @return All complete tags with this name in the page.
   */
  public List<PageElementTag> getCompleteTags(String name) {
    if (name == null) {
      return null;
    }
    synchronized (lockTagsByName) {
      if (completeTagsByName == null) {
        completeTagsByName = new HashMap<String, List<PageElementTag>>();
      }
      name = name.toLowerCase();
      List<PageElementTag> result = completeTagsByName.get(name);
      if (result == null) {
        List<PageElementTag> tmpTags = getTags(name);
        result = new ArrayList<PageElementTag>();
        completeTagsByName.put(name, result);
        for (PageElementTag tag : tmpTags) {
          if (tag.isFullTag()) {
            result.add(tag);
          } else if (!tag.isEndTag() && tag.isComplete()) {
            result.add(tag);
          }
        }
      }
      return result;
    }
  }

  /**
   * @param name Tag name.
   * @param currentIndex Current index.
   * @return Surrounding tag.
   */
  public PageElementTag getSurroundingTag(String name, int currentIndex) {
    List<PageElementTag> tmpTags = getCompleteTags(name);
    if (tmpTags == null) {
      return null;
    }
    PageElementTag result = null;
    for (PageElementTag tag : tmpTags) {
      if ((!tag.isFullTag()) &&
          (tag.getValueBeginIndex() <= currentIndex) &&
          (tag.getValueEndIndex() > currentIndex)) {
        if ((result == null) ||
            (tag.getBeginIndex() > result.getBeginIndex())) {
          result = tag;
        }
      }
    }
    return result;
  }

  /**
   * @param currentIndex Current index.
   * @return Next tag.
   */
  public PageElementTag getNextTag(int currentIndex) {
    List<PageElementTag> tmpTags = getTags();
    for (PageElementTag tag : tmpTags) {
      if (tag.getBeginIndex() >= currentIndex) {
        return tag;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Tag if the current index is inside a tag.
   */
  public PageElementTag isInTag(int currentIndex) {
    List<PageElementTag> tmpTags = getTags();
    for (PageElementTag tag : tmpTags) {
      if ((tag.getBeginIndex() <= currentIndex) &&
          (tag.getEndIndex() > currentIndex)) {
        return tag;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @param tagName Tag name.
   * @return Tag if the current index is inside a tag.
   */
  public PageElementTag isInTag(int currentIndex, String tagName) {
    List<PageElementTag> tmpTags = getTags(tagName);
    for (PageElementTag tag : tmpTags) {
      if ((tag.getBeginIndex() <= currentIndex) &&
          (tag.getEndIndex() > currentIndex)) {
        return tag;
      }
    }
    return null;
  }

  // ==========================================================================
  // DEFAULTSORT management
  // ==========================================================================

  /**
   * @return All DEFAULTSORT in the page.
   */
  public List<PageElementFunction> getDefaultSorts() {
    List<PageElementFunction> tmpFunctions = getFunctions();
    if (tmpFunctions == null) {
      return null;
    }
    List<PageElementFunction> defaultSorts = new ArrayList<PageElementFunction>();
    for (PageElementFunction function : tmpFunctions) {
      if (MagicWord.DEFAULT_SORT.equals(function.getMagicWord().getName())) {
        defaultSorts.add(function);
      }
    }
    return defaultSorts;
  }

  /**
   * @param currentIndex Current index.
   * @return DefaultSort if the current index is inside a DEFAULTSORT.
   */
  public PageElementFunction isInDefaultSort(int currentIndex) {
    List<PageElementFunction> tmpDefaultSorts = getDefaultSorts();
    for (PageElementFunction defaultSort : tmpDefaultSorts) {
      if ((defaultSort.getBeginIndex() <= currentIndex) &&
          (defaultSort.getEndIndex() > currentIndex)) {
        return defaultSort;
      }
    }
    return null;
  }

  // ==========================================================================
  // Categories management
  // ==========================================================================

  /**
   * All categories in the page.
   */
  private List<PageElementCategory> categories;

  /**
   * @return All categories in the page.
   */
  public List<PageElementCategory> getCategories() {
    level3Analysis();
    return categories;
  }

  /**
   * @param currentIndex Current index.
   * @return Next category.
   */
  public PageElementCategory getNextCategory(int currentIndex) {
    List<PageElementCategory> tmpCategories = getCategories();
    for (PageElementCategory category : tmpCategories) {
      if (category.getBeginIndex() >= currentIndex) {
        return category;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Category if the current index is inside a category.
   */
  public PageElementCategory isInCategory(int currentIndex) {
    List<PageElementCategory> tmpCategories = getCategories();
    for (PageElementCategory category : tmpCategories) {
      if ((category.getBeginIndex() <= currentIndex) &&
          (category.getEndIndex() > currentIndex)) {
        return category;
      }
    }
    return null;
  }

  // ==========================================================================
  // Interwiki links management
  // ==========================================================================

  /**
   * All interwiki links in the page.
   */
  private List<PageElementInterwikiLink> interwikiLinks;

  /**
   * @return All interwiki links in the page.
   */
  public List<PageElementInterwikiLink> getInterwikiLinks() {
    level3Analysis();
    return interwikiLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next interwiki link.
   */
  public PageElementInterwikiLink getNextInterwikiLink(int currentIndex) {
    List<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
    for (PageElementInterwikiLink link : tmpLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Interwiki link if the current index is inside an interwiki link.
   */
  public PageElementInterwikiLink isInInterwikiLink(int currentIndex) {
    List<PageElementInterwikiLink> tmpLinks = getInterwikiLinks();
    for (PageElementInterwikiLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  // ==========================================================================
  // Language links management
  // ==========================================================================

  /**
   * All language links in the page.
   */
  private List<PageElementLanguageLink> languageLinks;

  /**
   * @return All language links in the page.
   */
  public List<PageElementLanguageLink> getLanguageLinks() {
    level3Analysis();
    return languageLinks;
  }

  /**
   * @param currentIndex Current index.
   * @return Next language link.
   */
  public PageElementLanguageLink getNextLanguageLink(int currentIndex) {
    List<PageElementLanguageLink> tmpLinks = getLanguageLinks();
    for (PageElementLanguageLink link : tmpLinks) {
      if (link.getBeginIndex() >= currentIndex) {
        return link;
      }
    }
    return null;
  }

  /**
   * @param currentIndex Current index.
   * @return Language link if the current index is inside a language link.
   */
  public PageElementLanguageLink isInLanguageLink(int currentIndex) {
    List<PageElementLanguageLink> tmpLinks = getLanguageLinks();
    for (PageElementLanguageLink link : tmpLinks) {
      if ((link.getBeginIndex() <= currentIndex) &&
          (link.getEndIndex() > currentIndex)) {
        return link;
      }
    }
    return null;
  }

  // ==========================================================================
  // ISBN, ISSN, PMID and RFC management
  // ==========================================================================

  /**
   * All ISBNs in the page
   */
  private List<PageElementISBN> isbns;

  /**
   * @return All ISBNs in the page.
   */
  public List<PageElementISBN> getISBNs() {
    level5Analysis();
    return isbns;
  }

  /**
   * @param currentIndex Current index.
   * @return ISBN if the current index is inside an ISBN.
   */
  public PageElementISBN isInISBN(int currentIndex) {
    List<PageElementISBN> tmpIsbns = getISBNs();
    for (PageElementISBN isbn : tmpIsbns) {
      if ((isbn.getBeginIndex() <= currentIndex) &&
          (isbn.getEndIndex() > currentIndex)) {
        return isbn;
      }
    }
    return null;
  }


  /**
   * All ISSNs in the page
   */
  private List<PageElementISSN> issns;

  /**
   * @return All ISSNs in the page.
   */
  public List<PageElementISSN> getISSNs() {
    level5Analysis();
    return issns;
  }

  /**
   * @param currentIndex Current index.
   * @return ISSN if the current index is inside an ISSN.
   */
  public PageElementISSN isInISSN(int currentIndex) {
    List<PageElementISSN> tmpIsbns = getISSNs();
    for (PageElementISSN issn : tmpIsbns) {
      if ((issn.getBeginIndex() <= currentIndex) &&
          (issn.getEndIndex() > currentIndex)) {
        return issn;
      }
    }
    return null;
  }

  
  /**
   * All PMIDs in the page
   */
  private List<PageElementPMID> pmids;

  /**
   * @return All PMIDs in the page.
   */
  public List<PageElementPMID> getPMIDs() {
    level5Analysis();
    return pmids;
  }

  /**
   * @param currentIndex Current index.
   * @return PMID if the current index is inside a PMID.
   */
  public PageElementPMID isInPMID(int currentIndex) {
    List<PageElementPMID> tmpPmids = getPMIDs();
    for (PageElementPMID pmid : tmpPmids) {
      if ((pmid.getBeginIndex() <= currentIndex) &&
          (pmid.getEndIndex() > currentIndex)) {
        return pmid;
      }
    }
    return null;
  }

  
  /**
   * All RFCs in the page
   */
  private List<PageElementRFC> rfcs;

  /**
   * @return All RFCs in the page.
   */
  public List<PageElementRFC> getRFCs() {
    level5Analysis();
    return rfcs;
  }

  /**
   * @param currentIndex Current index.
   * @return RFC if the current index is inside a RFC.
   */
  public PageElementRFC isInRFC(int currentIndex) {
    List<PageElementRFC> tmpRfcs = getRFCs();
    for (PageElementRFC rfc : tmpRfcs) {
      if ((rfc.getBeginIndex() <= currentIndex) &&
          (rfc.getEndIndex() > currentIndex)) {
        return rfc;
      }
    }
    return null;
  }

  // ==========================================================================
  // Paragraphs management
  // ==========================================================================

  /** All paragraphs in the page */
  private List<PageElementParagraph> paragraphs;

  /**
   * @return All paragraphs in the page.
   */
  public List<PageElementParagraph> getParagraphs() {
    level6Analysis();
    return paragraphs;
  }

  /**
   * @param currentIndex Current index.
   * @return Paragraph if the current index is inside a paragraph.
   */
  public PageElementParagraph isInParagraph(int currentIndex) {
    return PageElementParagraph.isInParagraph(currentIndex, getParagraphs());
  }

  // ==========================================================================
  // Lists management
  // ==========================================================================

  /** All list items in the page */
  private List<PageElementListItem> listItems;

  /**
   * @return All list items in the page.
   */
  public List<PageElementListItem> getListItems() {
    level6Analysis();
    return listItems;
  }

  /**
   * @param currentIndex Current index.
   * @return List item if the current index is inside a list item.
   */
  public PageElementListItem isInListItem(int currentIndex) {
    return PageElementListItem.isInListItem(currentIndex, getListItems());
  }

  // ==========================================================================
  // Tables management
  // ==========================================================================

  /** All tables in the page */
  private List<PageElementTable> tables;

  /**
   * @return All table in the page.
   */
  public List<PageElementTable> getTables() {
    level6Analysis();
    return tables;
  }

  /**
   * @param currentIndex Current index.
   * @return Table if the current index is inside a table.
   */
  public PageElementTable isInTable(int currentIndex) {
    return PageElementTable.isInTable(currentIndex, getTables());
  }

  // ==========================================================================
  // Errors management
  // ==========================================================================

  /**
   * Bean for holding results about error detection.
   */
  public static class Result {

    /**
     * True if errors of this kind have been found.
     */
    private final boolean found;

    /**
     * List of errors found.
     */
    private final List<CheckErrorResult> errors;

    Result(boolean found, List<CheckErrorResult> errors) {
      this.found = found;
      this.errors = (errors != null) ? new ArrayList<CheckErrorResult>(errors) : null;
    }

    /**
     * @param results (Out) List of errors found.
     * @return True if errors of this kind have been found.
     */
    public boolean getErrors(List<CheckErrorResult> results) {
      if ((results != null) && (errors != null)) {
        results.addAll(errors);
      }
      return found;
    }
  }

  /**
   * Memorizing Check Wiki errors.
   */
  private Map<Integer, Result> checkWikiErrors;

  /**
   * Memorize Check Wiki errors.
   * 
   * @param errorNumber Error number.
   * @param found True if errors of this kind have been found.
   * @param errors List of errors found.
   */
  public void setCheckWikiErrors(int errorNumber, boolean found, List<CheckErrorResult> errors) {
    if (checkWikiErrors == null) {
      checkWikiErrors = new HashMap<Integer, PageAnalysis.Result>();
    }
    checkWikiErrors.put(Integer.valueOf(errorNumber), new Result(found, errors));
  }

  /**
   * @param errorNumber Error number.
   * @return Errors for this error number.
   */
  public Result getCheckWikiErrors(int errorNumber) {
    if (checkWikiErrors == null) {
      return null;
    }
    return checkWikiErrors.get(Integer.valueOf(errorNumber));
  }
}
