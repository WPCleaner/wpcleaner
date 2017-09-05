/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 64 of check wikipedia project.
 * Error 64: Link equal to link text
 */
public class CheckErrorAlgorithm064 extends CheckErrorAlgorithmBase {

  /** Possible global fixes */
  private final static String[] globalFixes = new String[] {
    GT._("Modify all internal links"),
  };

  /** Possible quotes before text */
  private final static String POSSIBLE_QUOTES_BEFORE = "«'`‘\"„“” .,(–־—";

  /** Possible quotes after text */
  private final static String POSSIBLE_QUOTES_AFTER = "»'`‘\"„“” .,);:–־—";

  public CheckErrorAlgorithm064() {
    super("Link equal to linktext");
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

    // Check every internal link
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    if ((links == null) || (links.isEmpty())) {
      return false;
    }
    boolean result = false;
    String content = analysis.getContents();
    for (PageElementInternalLink link : links) {

      // Analyze
      String anchor = link.getAnchor();
      String linkName = link.getLink();
      String text = link.getText();
      String cleanedText = (text != null) ? text.replaceAll("\\_", " ") : text;
      String paddingLeft = "";
      String paddingRight = "";
      boolean same = false;
      boolean automatic = false;
      if (((anchor == null) || (anchor.trim().length() == 0)) &&
          (text != null)) {

        // Check for exact same title
        if (!same) {
          if (Page.areSameTitle(linkName, text)) {
            same = true;
            automatic = true;
          } else if (Page.areSameTitle(linkName, cleanedText)) {
            same = true;
          }
        }

        // Check for extra <br> tag at the end
        if (!same) {
          int tmpIndex = link.getEndIndex() - 3;
          while ((tmpIndex >= 0) && (content.charAt(tmpIndex) == ' ')) {
            tmpIndex--;
          }
          if (content.charAt(tmpIndex) == '>') {
            PageElementTag brTag = analysis.isInTag(tmpIndex, PageElementTag.TAG_HTML_BR);
            if (brTag != null) {
              String cutText = text.substring(
                  0,
                  text.length() - link.getEndIndex() + tmpIndex + 4 - brTag.getEndIndex() + brTag.getBeginIndex()).trim();
              if ((cutText != null) && (Page.areSameTitle(linkName, cutText))) {
                same = true;
                text = cutText;
                cleanedText = text;
              }
            }
          }
        }

        // Check for extra characters around or before
        if (!same) {
          int countQuoteBefore = 0;
          while ((countQuoteBefore < text.length()) &&
                 (POSSIBLE_QUOTES_BEFORE.indexOf(text.charAt(countQuoteBefore)) >= 0)) {
            countQuoteBefore++;
          }
          int countQuoteAfter = 0;
          while ((countQuoteAfter < text.length()) &&
                 (POSSIBLE_QUOTES_AFTER.indexOf(text.charAt(text.length() - countQuoteAfter - 1)) >= 0)) {
            countQuoteAfter++;
          }
          if (((countQuoteBefore >= 1) || (countQuoteAfter >= 1)) &&
              (text.length() > countQuoteBefore + countQuoteAfter)) {
            paddingLeft = paddingLeft + text.substring(0, countQuoteBefore);
            paddingRight = text.substring(text.length() - countQuoteAfter) + paddingRight;
            text = text.substring(countQuoteBefore, text.length() - countQuoteAfter);
            cleanedText = (text != null) ? text.replaceAll("\\_", " ") : text;
            if (Page.areSameTitle(linkName, text)) {

              // Specific cases
              boolean shouldReport = true;
              if (".".equals(paddingLeft) && "".equals(paddingRight)) {
                shouldReport = false; // Only a "." before
              } else if ("".equals(paddingLeft) && "'".equals(paddingRight)) {
                shouldReport = false; // Only a "'" after
              }

              // Report problem
              if (shouldReport) {
                same = true;
                boolean risk = false;
                if ((link.getBeginIndex() > 0) &&
                    (content.charAt(link.getBeginIndex() - 1) == '\'') &&
                    (paddingLeft.length() > 0) &&
                    (paddingLeft.charAt(paddingLeft.length() - 1) == '\'')) {
                  risk = true;
                }
                if ((link.getEndIndex() < content.length()) &&
                    (content.charAt(link.getEndIndex()) == '\'') &&
                    (paddingRight.length() > 0) &&
                    (paddingRight.charAt(0) == '\'')) {
                  risk = true;
                }
                if ((paddingRight.length() > 0) &&
                    (paddingRight.charAt(0) == '‘')) {
                  risk = true;
                }
                Integer namespace = analysis.getPage().getNamespace();
                automatic = !risk && (namespace != null) && (namespace.intValue() == Namespace.MAIN);
              }
            } else if (Page.areSameTitle(linkName, cleanedText)) {
              same = true;
            }
          }
        }
      }

      // Report error
      if (same && (text != null)) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Analyze for possible extra modifications
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        String extraRight = "";
        String extraFullRight = "";
        if ((endIndex < content.length()) &&
            (content.charAt(endIndex) == '\'')) {
          int countQuoteBefore = 0;
          int tmpIndex = endIndex;
          while ((tmpIndex < content.length() &&
                 (content.charAt(tmpIndex) == '\''))) {
            tmpIndex++;
            countQuoteBefore++;
          }
          while ((tmpIndex < content.length()) &&
                 (" ,.:".indexOf(content.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
          }
          int countQuoteAfter = 0;
          while ((tmpIndex < content.length() &&
                 (content.charAt(tmpIndex) == '\''))) {
            tmpIndex++;
            countQuoteAfter++;
          }
          if (countQuoteBefore == countQuoteAfter) {
            extraRight = content.substring(endIndex + countQuoteBefore, tmpIndex - countQuoteAfter);
            extraFullRight = content.substring(endIndex, tmpIndex);
            endIndex = tmpIndex;
          }
        }

        // Analyze for possible extra quotes around the link
        int fullBeginIndex = beginIndex;
        int fullEndIndex = endIndex;
        while ((fullBeginIndex > 0) && (content.charAt(fullBeginIndex - 1) == '\'')) {
          fullBeginIndex--;
        }
        while ((fullEndIndex < content.length()) && (content.charAt(fullEndIndex) == '\'')) {
          fullEndIndex++;
        }
        String prefix = (fullBeginIndex < beginIndex) ? content.substring(fullBeginIndex, beginIndex) : "";
        String suffix = (fullEndIndex > endIndex) ? content.substring(endIndex, fullEndIndex) : "";
        WPCConfiguration config = analysis.getWPCConfiguration();

        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, fullBeginIndex, fullEndIndex);
        List<String> replacements = new ArrayList<>();
        if (!automatic) {
          addReplacement(
              config, errorResult, replacements,
              paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraRight,
              prefix, suffix, false);
        }
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraFullRight,
            prefix, suffix, automatic);
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraRight,
            prefix, suffix, false);
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraFullRight,
            prefix, suffix, false);
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraRight,
            prefix, suffix, false);
        paddingLeft = paddingLeft.replaceAll("\'", "");
        paddingRight = paddingRight.replaceAll("\'", "");
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraFullRight,
            prefix, suffix, false);
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(text, null) + paddingRight + extraRight,
            prefix, suffix, false);
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraFullRight,
            prefix, suffix, false);
        addReplacement(
            config, errorResult, replacements,
            paddingLeft + PageElementInternalLink.createInternalLink(cleanedText, null) + paddingRight + extraRight,
            prefix, suffix, false);
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Add a bunch of replacements.
   * 
   * @param errorResult Error.
   * @param replacements List of existing replacements.
   * @param replacement Replacement.
   * @param prefix Prefix for the replacement.
   * @param suffix Suffix for the replacement.
   * @param automatic True if replacement is automatic.
   */
  private void addReplacement(
      WPCConfiguration config,
      CheckErrorResult errorResult, List<String> replacements,
      String replacement, String prefix, String suffix, boolean automatic) {
    boolean apostrophe =
        prefix.contains("'") || suffix.contains("'") ||
        replacement.startsWith("'") || replacement.endsWith("'");
    if (automatic || !apostrophe) {
      addReplacement(errorResult, replacements, prefix + replacement + suffix, automatic);
    }
    String apostropheTemplate = config.getString(
        WPCConfigurationString.APOSTROPHE_TEMPLATE);
    if ((apostropheTemplate != null) && (apostropheTemplate.length() > 0)) {
      String replacedPrefix = prefix.replaceAll("\\'", PageElementTemplate.createTemplate(apostropheTemplate));
      String replacedSuffix = suffix.replaceAll("\\'", PageElementTemplate.createTemplate(apostropheTemplate));
      addReplacement(errorResult, replacements, replacedPrefix + replacement + replacedSuffix, false);
      addReplacement(errorResult, replacements, replacedPrefix + replacement + suffix, false);
      addReplacement(errorResult, replacements, prefix + replacement + replacedSuffix, false);
    }
    addReplacement(errorResult, replacements, prefix + replacement + suffix, automatic);
  }

  /**
   * Add a replacement.
   * 
   * @param errorResult Error.
   * @param replacements List of existing replacements.
   * @param replacement Replacement.
   * @param automatic True if replacement is automatic.
   */
  private void addReplacement(
      CheckErrorResult errorResult, List<String> replacements,
      String replacement, boolean automatic) {
    if (!replacements.contains(replacement)) {
      errorResult.addReplacement(replacement, automatic);
      replacements.add(replacement);
    }
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
