/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementFullTag;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.IntervalComparator;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 61 of check wikipedia project.
 * Error 61: Reference with punctuation
 */
public class CheckErrorAlgorithm061 extends CheckErrorAlgorithmBase {

  /** Punctuation characters looked for by the analysis */
  private static final String PUNCTUATION = ",.;:";

  public CheckErrorAlgorithm061() {
    super("Reference before punctuation");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Analyze from the beginning
    List<PageElement> refs = getRefs(analysis);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    int refIndex = 0;
    int maxRefs = refs.size();
    while (refIndex < maxRefs) {

      // Group references separated only by punctuation characters
      int lastRefIndex = PageElement.groupElements(refs, refIndex, contents, ",;.\'", separator);
      result |= analyzeGroupOfTags(analysis, contents, errors, refs, refIndex, lastRefIndex);
      refIndex = lastRefIndex + 1;
    }
    return result;
  }

  /**
   * Analyze a group of tags.
   * 
   * @param analysis Page analysis.
   * @param contents Page contents.
   * @param errors Errors found in the page.
   * @param refs List of references.
   * @param firstRefIndex Index of the first reference of the group.
   * @param lastRefIndex Index of the last reference of the group.
   * @return True if the error was found in the group of tags.
   */
  private boolean analyzeGroupOfTags(
      PageAnalysis analysis, String contents,
      Collection<CheckErrorResult> errors,
      List<PageElement> refs,
      int firstRefIndex, int lastRefIndex) {

    // Remove possible whitespace characters after last reference
    PageElement lastRef = refs.get(lastRefIndex);
    int tmpIndex = lastRef.getEndIndex();
    while (tmpIndex < contents.length()) {
      if (contents.charAt(tmpIndex) == '\n') {
        if ((tmpIndex + 1 < contents.length()) &&
            ((Character.isWhitespace(contents.charAt(tmpIndex + 1))) ||
             (contents.charAt(tmpIndex + 1) == '*') || // List
             (contents.charAt(tmpIndex + 1) == '#') || // List
             (contents.charAt(tmpIndex + 1) == ';') || // Definition
             (contents.charAt(tmpIndex + 1) == ':') || // Definition
             (contents.charAt(tmpIndex + 1) == '!'))) { // Table heading
          break;
        }
        tmpIndex++;
      } else if (Character.isWhitespace(contents.charAt(tmpIndex))) {
        tmpIndex++;
      } else {
        break;
      }
    }

    // Check if next character is a punctuation
    if (tmpIndex >= contents.length()) {
      return false;
    }
    char punctuation = ' ';
    punctuation = contents.charAt(tmpIndex);
    if (PUNCTUATION.indexOf(punctuation) < 0) {
      return false;
    }
    // TODO: Once tables are managed by parser, remove the this trick that prevent detection before "!!"
    if ((punctuation == '!') &&
        (tmpIndex + 1 < contents.length()) &&
        (contents.charAt(tmpIndex + 1) == punctuation)) {
      return false;
    }
    int firstPunctuationIndex = tmpIndex;

    // Check if the punctuation after is multiple
    tmpIndex++;
    while ((tmpIndex < contents.length()) &&
           (contents.charAt(tmpIndex) == punctuation)) {
      tmpIndex++;
    }

    // Check if error should be reported
    if (tmpIndex < contents.length()) {
      char nextChar = contents.charAt(tmpIndex);
      if (!Character.isWhitespace(nextChar)) {
        return false;
      }
    }
    PageElement firstRef = refs.get(firstRefIndex);
    int beginIndex = firstRef.getBeginIndex();
    int endIndex = tmpIndex;
    if (analysis.comments().isAt(beginIndex) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_CHEM) != null) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_MATH) != null) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_MATH_CHEM) != null) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_NOWIKI) != null) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_SCORE) != null) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_SOURCE) != null) ||
        (analysis.isInTag(beginIndex, PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT) != null)) {
      return false;
    }

    // Error found
    if (errors == null) {
      return true;
    }

    String punctuationAfter = contents.substring(firstPunctuationIndex, endIndex);

    // Construct list of tags
    String replace = PageElement.createListOfElements(
        refs, firstRefIndex, lastRefIndex, contents, separator);
    String textReplace = createReducedListOfRefs(
        lastRefIndex - firstRefIndex + 1, separator);

    // Check for possible punctuation before tags
    tmpIndex = beginIndex - 1;
    while ((tmpIndex >= 0) &&
           (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex--;
    }
    beginIndex = tmpIndex + 1;
    boolean punctuationFoundBefore = false;
    int punctuationBeforeIndex = tmpIndex;
    while ((tmpIndex >= 0) &&
           CharacterUtils.isPunctuation(contents.charAt(tmpIndex))) {
      punctuationFoundBefore = true;
      tmpIndex--;
    }
    String punctuationBefore = contents.substring(tmpIndex + 1, punctuationBeforeIndex + 1);
    if (punctuationFoundBefore) {
      beginIndex = tmpIndex + 1;
    }

    // Decide if automatic modifications can be applied
    boolean automatic = true;
    if (punctuationAfter.length() > 1) {
      automatic = false;
    }
    String allPunctutation = punctuationAfter;
    if (punctuationFoundBefore) {
      automatic = false;

      // Special cases where automatic modification can be kept
      if ((endIndex >= contents.length()) ||
          (" \n".indexOf(contents.charAt(endIndex)) >= 0)) {
        if (safeAfter.contains(punctuationAfter)) {
          if (punctuationAfter.equals(punctuationBefore)) {
            automatic = true;
          } else if (!punctuationBefore.contains(punctuationAfter) &&
                     keepBeforeIfDifferent.contains(punctuationBefore)) {
            automatic = true;
            allPunctutation = punctuationBefore + punctuationAfter;
          }
        }
      }
    }

    // Create error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errorResult.addReplacement(
        allPunctutation + replace,
        allPunctutation + textReplace,
        automatic);
    if (!punctuationAfter.equals(allPunctutation)) {
      errorResult.addReplacement(
          punctuationAfter + replace,
          punctuationAfter + textReplace);
    }
    if (punctuationFoundBefore &&
        !punctuationAfter.equals(punctuationBefore)) {
      errorResult.addReplacement(
          punctuationBefore + replace,
          punctuationBefore + textReplace);
    }
    errors.add(errorResult);

    return true;
  }

  /** List of punctuation after the reference that are safe to move */
  private final static Set<String> safeAfter = new HashSet<>(Arrays.asList(".", ",", ":", ";"));

  /** List of punctuation before the reference that should be kept if they don't contain the punctuation after */
  private final static Set<String> keepBeforeIfDifferent = new HashSet<>(Arrays.asList(")", "\"", "\")", ")\"", "?\""));
  
  /**
   * @param analysis Page analysis.
   * @return List of references (tags, templates, ...).
   */
  private List<PageElement> getRefs(PageAnalysis analysis) {
    List<PageElement> refs = new ArrayList<PageElement>();

    // Retrieve references defined by tags
    List<PageElementTag> refTags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if (refTags != null) {
      for (PageElementTag refTag : refTags) {
        refs.add(new PageElementFullTag(refTag));
      }
    }

    // Retrieve references defined by templates
    if (!templatesName.isEmpty()) {
      List<PageElementTemplate> templates = analysis.getTemplates();
      for (PageElementTemplate template : templates) {
        if (templatesName.contains(template.getTemplateName())) {
          refs.add(template);
        }
      }
    }

    Collections.sort(refs, new IntervalComparator());
    return refs;
  }

  /**
   * Create a reduced textual representation of a list of references.
   * 
   * @param count Number of references in the list.
   * @param separator Separator.
   * @return Reduced textual representation of a list of references.
   */
  public static String createReducedListOfRefs(
      int count, String separator) {
    if (count > 2) {
      return "<ref>...</ref>" + separator + "..." + separator + "<ref>...</ref>";
    }
    if (count > 1) {
      return "<ref>...</ref>" + separator + "<ref>...</ref>";
    }
    return "<ref>...</ref>";
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Separator between consecutive tags */
  private static final String PARAMETER_SEPARATOR = "separator";

  /** Templates that can replace a tag */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    separator = getSpecificProperty(PARAMETER_SEPARATOR, true, false, false);
    if (separator == null) {
      separator = "";
    }

    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templatesName.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      for (String tmpElement : tmpList) {
        templatesName.add(Page.normalizeTitle(tmpElement));
      }
    }
  }

  /** Separator between consecutive tags */
  private String separator = "";

  /** Templates that can replace a tag */
  private final Set<String> templatesName = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_SEPARATOR,
        GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"),
        new AlgorithmParameterElement(
            "text",
            GT._T("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that can be used to replace {0} tags", "&lt;ref&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template that can be used to replace {0} tags", "&lt;ref&gt;")),
        true));
  }
}
