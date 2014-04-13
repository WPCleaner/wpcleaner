/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementComparator;
import org.wikipediacleaner.api.data.PageElementFullTag;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 61 of check wikipedia project.
 * Error 61: Reference with punctuation
 */
public class CheckErrorAlgorithm061 extends CheckErrorAlgorithmBase {

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
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Retrieve separator between several <ref> tags
    String separator = getSpecificProperty(
        "separator", true, false, false);
    if (separator == null) {
      separator = "";
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
      int firstRefIndex = refIndex;
      PageElement firstRef = refs.get(firstRefIndex);
      int lastRefIndex = PageElement.groupElements(refs, firstRefIndex, contents, ",;.\'", separator);
      PageElement lastRef = refs.get(lastRefIndex);
      refIndex = lastRefIndex + 1;

      // Remove possible whitespace characters after last reference
      int tmpIndex = lastRef.getEndIndex();
      boolean finished = false;
      while (!finished) {
        if (tmpIndex >= contents.length()) {
          finished = true;
        } else if (contents.charAt(tmpIndex) == '\n') {
          if ((tmpIndex + 1 < contents.length()) &&
              ((Character.isWhitespace(contents.charAt(tmpIndex + 1))) ||
               (contents.charAt(tmpIndex + 1) == '*') || // List
               (contents.charAt(tmpIndex + 1) == '#') || // List
               (contents.charAt(tmpIndex + 1) == ';') || // Definition
               (contents.charAt(tmpIndex + 1) == ':') || // Definition
               (contents.charAt(tmpIndex + 1) == '!'))) { // Table heading
            finished = true;
          } else {
            tmpIndex++;
          }
        } else if (Character.isWhitespace(contents.charAt(tmpIndex))) {
          tmpIndex++;
        } else {
          finished = true;
        }
      }

      // Check if next character is a punctuation
      boolean punctuationFound = false;
      char punctuation = ' ';
      if (tmpIndex < contents.length()) {
        punctuation = contents.charAt(tmpIndex);
        if (SpecialCharacters.isPunctuation(punctuation)) {
          // TODO: Once tables are managed by parser, remove the this trick that prevent detection before "!!"
          if ((punctuation != '!') ||
              (tmpIndex + 1 >= contents.length()) ||
              (contents.charAt(tmpIndex + 1) != punctuation)) {
            punctuationFound = true;
          }
        }
      }

      // Punctuation found
      if (punctuationFound) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check if the punctuation after is multiple
        int firstPunctuationIndex = tmpIndex;
        while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == punctuation)) {
          tmpIndex++;
        }
        int beginIndex = firstRef.getBeginIndex();
        int endIndex = tmpIndex;
        String allPunctuations = contents.substring(firstPunctuationIndex, endIndex);

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
               SpecialCharacters.isPunctuation(contents.charAt(tmpIndex))) {
          punctuationFoundBefore = true;
          tmpIndex--;
        }
        String punctuationBefore = contents.substring(tmpIndex + 1, punctuationBeforeIndex + 1);
        if (punctuationFoundBefore) {
          beginIndex = tmpIndex + 1;
        }

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        errorResult.addReplacement(
            allPunctuations + replace,
            allPunctuations + textReplace);
        if (punctuationFoundBefore &&
            !allPunctuations.equals(punctuationBefore)) {
          errorResult.addReplacement(
              punctuationBefore + replace,
              punctuationBefore + textReplace);
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

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
    String templatesProp = getSpecificProperty("templates", true, true, false);
    if (templatesProp != null) {
      String[] templatesName = templatesProp.split("\n");
      for (String templateName : templatesName) {
        List<PageElementTemplate> templates = analysis.getTemplates(templateName.trim());
        if (templates != null) {
          refs.addAll(templates);
        }
      }
    }

    Collections.sort(refs, new PageElementComparator());
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

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "separator",
        GT._("Used as a separator between consecutive {0} tags", "&lt;ref&gt;"));
    parameters.put(
        "templates",
        GT._("Templates that can be used to replace {0} tags", "&lt;ref&gt;"));
    return parameters;
  }
}
