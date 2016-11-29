/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 2 of check wikipedia project.
 * Error 2: Article with incorrect tags (&lt;br&gt;, &lt;center&gt;, &lt;div&gt;, &lt;small&gt;, &lt;span&gt;...)
 */
public class CheckErrorAlgorithm002 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Fix all incorrect tags"),
  };

  public CheckErrorAlgorithm002() {
    super("Article with incorrect tags");
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

    // Check for true self closing tags
    boolean result = false;
    result |= analyzeSelfClosingTags(analysis, errors, PageElementTag.TAG_HTML_BR);
    result |= analyzeSelfClosingTags(analysis, errors, PageElementTag.TAG_HTML_HR);

    // Check for tags that should not be self closing
    // Valid HTML tags are only: area, base, br, col, embed, hr, img, input, keygen, link, meta, param, source, track, wbr
    String[] listTags = new String[] {
        PageElementTag.TAG_HTML_ABBR,
        PageElementTag.TAG_HTML_B,
        PageElementTag.TAG_HTML_BIG,
        PageElementTag.TAG_HTML_BLOCKQUOTE,
        PageElementTag.TAG_HTML_CENTER,
        PageElementTag.TAG_HTML_CITE,
        PageElementTag.TAG_HTML_CODE,
        PageElementTag.TAG_HTML_DEL,
        PageElementTag.TAG_HTML_DIV,
        PageElementTag.TAG_HTML_EM,
        PageElementTag.TAG_HTML_FONT,
        PageElementTag.TAG_HTML_H1,
        PageElementTag.TAG_HTML_H2,
        PageElementTag.TAG_HTML_H3,
        PageElementTag.TAG_HTML_H4,
        PageElementTag.TAG_HTML_H5,
        PageElementTag.TAG_HTML_H6,
        PageElementTag.TAG_HTML_H7,
        PageElementTag.TAG_HTML_H8,
        PageElementTag.TAG_HTML_H9,
        PageElementTag.TAG_HTML_I,
        PageElementTag.TAG_HTML_P,
        PageElementTag.TAG_HTML_S,
        PageElementTag.TAG_HTML_SMALL,
        PageElementTag.TAG_HTML_SPAN,
        PageElementTag.TAG_HTML_STRIKE,
        PageElementTag.TAG_HTML_SUB,
        PageElementTag.TAG_HTML_SUP,
        PageElementTag.TAG_HTML_TABLE,
        PageElementTag.TAG_HTML_TD,
        PageElementTag.TAG_HTML_TH,
        PageElementTag.TAG_HTML_TR,
        PageElementTag.TAG_HTML_TT,
        PageElementTag.TAG_HTML_U,
        PageElementTag.TAG_HTML_UL,
    };
    for (String tagName : listTags) {
      result |= analyzeNonFullTags(analysis, errors, tagName);
    }

    // Check for incorrectly written tags
    result |= analyzeIncorrectTags(analysis, errors, listTags);

    // Check for <cite> tags inside <ref> tags
    result |= analyzeCiteTags(analysis, errors);

    return result;
  }

  /**
   * Analyze a page to check if cite tags are incorrectly used.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeCiteTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementTag> citeTags = analysis.getTags(PageElementTag.TAG_HTML_CITE);
    if ((citeTags == null) || citeTags.isEmpty()) {
      return false;
    }

    // Check each cite tag
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTag citeTag : citeTags) {
      if (!citeTag.isEndTag()) {
        PageElementTag refTag = analysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_REF, citeTag.getBeginIndex());
        if ((refTag != null) && (refTag.getEndIndex() == citeTag.getBeginIndex())) {
          if (errors == null) {
            return true;
          }
          result = true;

          // Try to extend area
          boolean extended = false;
          int endIndex = citeTag.getCompleteEndIndex();
          do {
            extended = false;
            if (endIndex < contents.length()) {
              if (contents.charAt(endIndex) == '<') {
                PageElementTag nextTag = analysis.isInTag(endIndex);
                if ((nextTag != null) && !nextTag.isFullTag() && nextTag.isComplete()) {
                  if (PageElementTag.TAG_HTML_SPAN.equals(nextTag.getName())) {
                    Parameter title = nextTag.getParameter("title");
                    if ((title != null) &&
                        (title.getValue() != null) &&
                        (title.getValue().startsWith("ctx_ver="))) {
                      String nextTagValue = contents.substring(
                          nextTag.getValueBeginIndex(), nextTag.getValueEndIndex());
                      if ((nextTagValue == null) ||
                          nextTagValue.equals("&nbsp;") ||
                          nextTagValue.equals("&#x20;")) {
                        extended = true;
                        endIndex = nextTag.getCompleteEndIndex();
                      }
                    }
                  } else if (PageElementTag.TAG_HTML_CITE.equals(nextTag.getName())) {
                    String nextTagValue = contents.substring(
                        nextTag.getValueBeginIndex(), nextTag.getValueEndIndex());
                    if ((nextTagValue == null) ||
                        nextTagValue.trim().equals("")) {
                      extended = true;
                      endIndex = nextTag.getCompleteEndIndex();
                    }
                  }
                }
              }
            }
          } while (extended);
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, citeTag.getCompleteBeginIndex(), endIndex);
          String replacement = contents.substring(
              citeTag.getValueBeginIndex(), citeTag.getValueEndIndex());
          if (citeTag.getCompleteEndIndex() == refTag.getValueEndIndex()) {
            replacement = replacement.trim();
          }
          errorResult.addReplacement(
              replacement,
              GT._("Remove {0} tags", PageElementTag.TAG_HTML_CITE));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Analyze a page to check if full tags are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagName Tag name.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeNonFullTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String tagName) {

    // Retrieve configuration
    List<String[]> anchorTemplates = null;
    if (PageElementTag.TAG_HTML_CITE.equals(tagName) ||
        PageElementTag.TAG_HTML_DIV.equals(tagName) ||
        PageElementTag.TAG_HTML_SPAN.equals(tagName)) {
      String anchorProperty = getSpecificProperty("anchor_templates", true, true, false);
      if (anchorProperty != null) {
        anchorTemplates = WPCConfiguration.convertPropertyToStringArrayList(anchorProperty);
      }
    }

    // Check for tags
    boolean result = false;
    List<PageElementTag> tags = analysis.getTags(tagName);
    PageElementTag previousTag = null;
    for (PageElementTag tag : tags) {

      boolean shouldReport = false;
      int beginIndex = tag.getBeginIndex();
      if (tag.isFullTag()) {
        shouldReport = true;
      }
      if (shouldReport) {
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, beginIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, beginIndex) != null)) {
          shouldReport = false;
        }
      }

      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult =
            createCheckErrorResult(analysis, beginIndex, tag.getEndIndex());

        // Check for consecutive opening tags without matching closing tags
        if ((previousTag != null) &&
            !previousTag.isComplete() &&
            !previousTag.isEndTag()) {
          errorResult.addReplacement(PageElementTag.createTag(tagName, true, false));
        }

        // Check for clear tags (<div clear="..."/>)
        if (PageElementTag.TAG_HTML_DIV.equals(tagName)) {
          String clearValue = getClearValue(tag);
          if (clearValue != null) {
            String clearReplacement = getClearReplacement(clearValue);
            if (clearReplacement != null) {
              errorResult.addReplacement(clearReplacement, false);
            }
          }
        }

        // Check for id tags (<span id="..."/> or <div id="..."/>)
        if ((anchorTemplates != null) &&
            !anchorTemplates.isEmpty() &&
            (tag.isComplete() || !tag.isEndTag())) {
          String idAttribute = null;
          boolean hasOtherAttribute = false;
          for (int numAttribute = 0; numAttribute < tag.getParametersCount(); numAttribute++) {
            Parameter param = tag.getParameter(numAttribute);
            if ((param != null) && (param.getName() != null)) {
              if ("id".equals(param.getName())) {
                if ((param.getTrimmedValue() != null) &&
                    !"".equals(param.getTrimmedValue())) {
                  idAttribute = param.getTrimmedValue();
                }
              } else {
                hasOtherAttribute = true;
              }
            }
          }
          if ((idAttribute != null) && (idAttribute.length() > 0) && !hasOtherAttribute) {
            for (String[] anchorTemplate : anchorTemplates) {
              if ((anchorTemplate.length > 0) && (anchorTemplate[0].length() > 0)) {
                StringBuilder replacement = new StringBuilder();
                replacement.append("{{");
                replacement.append(anchorTemplate[0]);
                replacement.append("|");
                if ((anchorTemplate.length > 1) && !"1".equals(anchorTemplate[1])) {
                  replacement.append(anchorTemplate[1]);
                  replacement.append("=");
                }
                replacement.append(idAttribute);
                replacement.append("}}");
                errorResult.addReplacement(replacement.toString());
              }
            }
          }
        }

        errorResult.addReplacement("");
        errors.add(errorResult);
      }
      previousTag = tag;
    }

    return result;
  }

  /**
   * Analyze a page to check for incorrectly written tags.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagNames Tag names.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeIncorrectTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String[] tagNames) {

    boolean result = false;

    // Check for incorrectly written tags (</xxx/>)
    int currentIndex = 0;
    String contents = analysis.getContents();
    while ((currentIndex >= 0) && (currentIndex < contents.length())) {
      currentIndex = contents.indexOf('<', currentIndex);
      String selectedTagName = null;
      if (currentIndex >= 0) {
        int beginIndex = currentIndex;
        boolean ok = true;
        currentIndex++;
        if (ok) {
          currentIndex = getFirstIndexAfterSpace(contents, currentIndex);
          if ((currentIndex < contents.length()) &&
              contents.charAt(currentIndex) == '/') {
            currentIndex++;
          } else {
            ok = false;
          }
        }
        if (ok &&
            (currentIndex < contents.length())) {
          currentIndex = getFirstIndexAfterSpace(contents, currentIndex);
        }
        if (ok) {
          for (String tagName : tagNames) {
            int length = tagName.length();
            if ((selectedTagName == null) &&
                (currentIndex + length < contents.length()) &&
                tagName.equalsIgnoreCase(contents.substring(currentIndex, currentIndex + length)) &&
                !Character.isLetterOrDigit(contents.charAt(currentIndex + length))) {
              currentIndex += length;
              selectedTagName = tagName;
            }
          }
          if (selectedTagName == null) {
            ok = false;
          }
        }
        if (ok) {
          currentIndex = getFirstIndexAfterSpace(contents, currentIndex);
          if ((currentIndex < contents.length()) &&
              contents.charAt(currentIndex) == '/') {
            currentIndex++;
          } else {
            ok = false;
          }
        }
        if (ok) {
          currentIndex = getFirstIndexAfterSpace(contents, currentIndex);
          if ((currentIndex < contents.length()) &&
              contents.charAt(currentIndex) == '>') {
            currentIndex++;
          } else {
            ok = false;
          }
        }
        if (ok) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, currentIndex);
          errorResult.addReplacement(PageElementTag.createTag(selectedTagName, true, false));
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present in self closing tags.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagName Tag name.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeSelfClosingTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String tagName) {

    // Check for incorrect self closing tags
    boolean result = false;
    int currentIndex = 0;
    String contents = analysis.getContents();
    int maxSize = contents.length();
    while (currentIndex < maxSize) {
      int nextIndex = currentIndex + 1;
      boolean shouldCheck = true;

      // Check if we are in a comment
      if (shouldCheck) {
        PageElementComment comment = analysis.isInComment(currentIndex);
        if (comment != null) {
          shouldCheck = false;
          nextIndex = comment.getEndIndex();
        }
      }

      // Check if this is a self closing tag for the given name
      if ((shouldCheck) && (contents.charAt(currentIndex) == '<')) {
        int tmpIndex = getFirstIndexAfterSpace(contents, currentIndex + 1);
        boolean incorrectChar = false;
        while ((tmpIndex < maxSize) &&
               (" \\.,:?/\n|+&)(".indexOf(contents.charAt(tmpIndex)) >= 0)) {
          tmpIndex++;
          incorrectChar = true;
        }
        boolean selfClosingTag = true;
        for (int i = 0; i < tagName.length(); i++) {
          if ((tmpIndex >= maxSize) ||
              (Character.toUpperCase(contents.charAt(tmpIndex)) != Character.toUpperCase(tagName.charAt(i)))) {
            selfClosingTag = false;
          }
          tmpIndex++;
        }
        if ((tmpIndex < maxSize) && selfClosingTag) {
          char tmpChar = contents.charAt(tmpIndex);
          selfClosingTag = !Character.isUpperCase(tmpChar) && !Character.isLowerCase(tmpChar);
        }
        if ((tmpIndex < maxSize) && selfClosingTag) {
          tmpIndex = getFirstIndexAfter(contents, tmpIndex, " \n");
          while ((tmpIndex < maxSize) &&
                 (" \\.,:?\n|+&)(`".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
            incorrectChar = true;
          }
          tmpIndex = getFirstIndexAfter(contents, tmpIndex, " \n");
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '/')) {
            tmpIndex++;
          }
          tmpIndex = getFirstIndexAfter(contents, tmpIndex, " \n");
          while ((tmpIndex < maxSize) &&
                 (" \\.,:?/\n|+&)(`".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
            incorrectChar = true;
          }
          if (tmpIndex < maxSize) {
            boolean shouldReport = false;
            if (incorrectChar) {
              shouldReport = true;
            } else {
              if (contents.charAt(tmpIndex) != '>') {
                PageElementTag tag = analysis.isInTag(currentIndex, tagName);
                if (tag == null) {
                  shouldReport = true;
                }
              }
            }
            if (shouldReport) {
              if (errors == null) {
                return true;
              }
              result = true;
              boolean endsWithGT = (contents.charAt(tmpIndex) == '>');
              if (endsWithGT) {
                tmpIndex++;
              }
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, currentIndex, tmpIndex);
              errorResult.addReplacement(
                  PageElementTag.createTag(tagName, false, false),
                  endsWithGT && incorrectChar);
              errors.add(errorResult);
              nextIndex = tmpIndex;
            }
          }
        }
      }

      currentIndex = nextIndex;
    }

    // Check for self closing tags with extra characters
    if (PageElementTag.TAG_HTML_BR.equals(tagName)) {
      List<PageElementTag> tags = analysis.getTags(tagName);
      for (PageElementTag tag : tags) {
  
        // Check for "clear" attribute
        String clearValue = getClearValue(tag);
  
        // Check for extra characters before the self closing tag
        boolean extra = false;
        int beginIndex = tag.getBeginIndex();
        while ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '<')) {
          beginIndex--;
          extra = true;
        }
  
        // Check for extra characters after the self closing tag
        int endIndex = tag.getEndIndex();
        while ((endIndex < contents.length()) && (contents.charAt(endIndex) == '>')) {
          endIndex++;
          extra  = true;
        }
  
        if (extra || (clearValue != null) || (tag.getParametersCount() > 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex, ErrorLevel.WARNING);
          if (clearValue != null) {
            String clearReplacement = getClearReplacement(clearValue);
            if (clearReplacement != null) {
              errorResult.addReplacement(clearReplacement, !clearReplacement.isEmpty());
            }
          }
          if (extra || (tag.getParametersCount() > 0)) {
            errorResult.addReplacement(
                PageElementTag.createTag(tagName, false, false),
                false);
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param tag Tag.
   * @return Value of clear attribute (or equivalent).
   */
  protected String getClearValue(PageElementTag tag) {
    if (tag == null) {
      return null;
    }

    // Attribute clear
    Parameter clearParameter = tag.getParameter("clear");
    if (clearParameter != null) {
      return clearParameter.getTrimmedValue();
    }

    // Attribute style
    Parameter styleParameter = tag.getParameter("style");
    if (styleParameter != null) {
      String styleValue = styleParameter.getTrimmedValue();
      final String prefix = "clear:";
      if ((styleValue != null) && styleValue.startsWith(prefix)) {
        String clearValue = styleValue.substring(prefix.length()).trim();
        while ((clearValue.length() > 0) && (clearValue.endsWith(";"))) {
          clearValue = clearValue.substring(0, clearValue.length() - 1);
        }
        return clearValue;
      }
    }

    // Attribute break;
    Parameter breakParameter = tag.getParameter("break");
    if (breakParameter != null) {
      return breakParameter.getTrimmedValue();
    }

    return null;
  }

  /**
   * @param clearValue Value of clear attribute.
   * @return Replacement for this value of clear attribute.
   */
  protected String getClearReplacement(String clearValue) {
    String clearReplacementName = null;
    if ("all".equalsIgnoreCase(clearValue) || "both".equalsIgnoreCase(clearValue)) {
      clearReplacementName = "clear_all";
    } else if ("left".equalsIgnoreCase(clearValue)) {
      clearReplacementName = "clear_left";
    } else if ("right".equalsIgnoreCase(clearValue)) {
      clearReplacementName = "clear_right";
    } else {
      return null;
    }
    return getSpecificProperty(clearReplacementName, true, true, false);
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

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("anchor_templates", GT._("A replacement for {0}", "&lt;span id=\"xxx\"/&gt;"));
    parameters.put("clear_all", GT._("A replacement for {0}", "&lt;br clear=\"all\"/&gt;"));
    parameters.put("clear_left", GT._("A replacement for {0}", "&lt;br clear=\"left\"/&gt;"));
    parameters.put("clear_right", GT._("A replacement for {0}", "&lt;br clear=\"right\"/&gt;"));
    return parameters;
  }
}
