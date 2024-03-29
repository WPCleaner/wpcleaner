/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a00x.a002;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagBuilder;
import org.wikipediacleaner.api.data.contents.tag.TagFormat;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
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
    GT._T("Fix all incorrect tags"),
  };

  /**
   * List of non self closing tags that should be verified.
   * 
   * Valid HTML tags are only: area, base, br, col, embed, hr, img, input, keygen, link, meta, param, source, track, wbr
   */
  private final static Set<TagType> nonSelfClosingTags = new HashSet<>();

  /**
   * Known erroneous tags that can be replaced automatically.
   */
  private final static Set<String> erroneousTags = new HashSet<>();

  static {
      // TODO: Use HtmlTagType attributes for filtering the list of all tags who can't be self closed
      nonSelfClosingTags.add(HtmlTagType.ABBR);
      nonSelfClosingTags.add(HtmlTagType.B);
      nonSelfClosingTags.add(HtmlTagType.BIG);
      nonSelfClosingTags.add(HtmlTagType.BLOCKQUOTE);
      nonSelfClosingTags.add(HtmlTagType.CENTER);
      nonSelfClosingTags.add(HtmlTagType.CITE);
      nonSelfClosingTags.add(HtmlTagType.CODE);
      nonSelfClosingTags.add(HtmlTagType.DEL);
      nonSelfClosingTags.add(HtmlTagType.DFN);
      nonSelfClosingTags.add(HtmlTagType.DIV);
      nonSelfClosingTags.add(HtmlTagType.EM);
      nonSelfClosingTags.add(HtmlTagType.FONT);
      nonSelfClosingTags.add(HtmlTagType.H1);
      nonSelfClosingTags.add(HtmlTagType.H2);
      nonSelfClosingTags.add(HtmlTagType.H3);
      nonSelfClosingTags.add(HtmlTagType.H4);
      nonSelfClosingTags.add(HtmlTagType.H5);
      nonSelfClosingTags.add(HtmlTagType.H6);
      nonSelfClosingTags.add(HtmlTagType.H7);
      nonSelfClosingTags.add(HtmlTagType.H8);
      nonSelfClosingTags.add(HtmlTagType.H9);
      nonSelfClosingTags.add(HtmlTagType.I);
      nonSelfClosingTags.add(HtmlTagType.P);
      nonSelfClosingTags.add(HtmlTagType.S);
      nonSelfClosingTags.add(HtmlTagType.SMALL);
      nonSelfClosingTags.add(HtmlTagType.SPAN);
      nonSelfClosingTags.add(HtmlTagType.STRIKE);
      nonSelfClosingTags.add(HtmlTagType.SUB);
      nonSelfClosingTags.add(HtmlTagType.SUP);
      nonSelfClosingTags.add(HtmlTagType.TABLE);
      nonSelfClosingTags.add(HtmlTagType.TD);
      nonSelfClosingTags.add(HtmlTagType.TH);
      nonSelfClosingTags.add(HtmlTagType.TR);
      nonSelfClosingTags.add(HtmlTagType.TT);
      nonSelfClosingTags.add(HtmlTagType.U);
      nonSelfClosingTags.add(HtmlTagType.UL);

      erroneousTags.add("</br>");
  }

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
    result |= analyzeSelfClosingTags(analysis, errors, HtmlTagType.BR);
    result |= analyzeSelfClosingTags(analysis, errors, HtmlTagType.HR);

    // Check for tags that should not be self closing
    result |= analyzeNonFullTags(analysis, errors);

    // Check for incorrectly written tags
    result |= analyzeIncorrectTags(analysis, errors, nonSelfClosingTags);

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
      Collection<CheckErrorResult> errors) {

    // Analyze each tag
    boolean result = false;
    for (PageElementTag tag : analysis.getTags()) {
      result |= analyzeNonFullTag(analysis, errors, tag);
    }
    return result;
  }

  /**
   * Analyze tag to check if it's a full.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagName Tag name.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeNonFullTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag) {

    // Keep only full tags for configured tags
    if (!tag.isFullTag()) {
      return false;
    }
    if (!nonSelfClosingTags.contains(tag.getType())) {
      return false;
    }

    // Ignore some situations
    int beginIndex = tag.getBeginIndex();
    if ((analysis.getSurroundingTag(WikiTagType.SOURCE, beginIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, beginIndex) != null)) {
      return false;
    }
    if (!WikiTagType.NOWIKI.equals(tag.getType()) &&
        (analysis.getSurroundingTag(WikiTagType.NOWIKI, beginIndex) != null)) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }

    // Retrieve configuration
    List<String[]> tmpAnchorTemplates = null;
    if (HtmlTagType.CITE.equals(tag.getType()) ||
        HtmlTagType.DIV.equals(tag.getType()) ||
        HtmlTagType.SPAN.equals(tag.getType())) {
      tmpAnchorTemplates = anchorTemplates;
    }

    // Create error
    CheckErrorResult errorResult =
        createCheckErrorResult(analysis, beginIndex, tag.getEndIndex());

    // Check for consecutive opening tags without matching closing tags
    PageElementTag previousTag = null;
    List<PageElementTag> otherTags = analysis.getTags(tag.getType());
    if (otherTags != null) {
      for (PageElementTag otherTag : otherTags) {
        if (otherTag.getEndIndex() <= tag.getBeginIndex()) {
          previousTag = otherTag;
        }
      }
    }
    if ((previousTag != null) &&
        !previousTag.isComplete() &&
        !previousTag.isEndTag()) {
      errorResult.addReplacement(
          TagBuilder.from(tag.getType(), TagFormat.CLOSE).toString());
    }

    // Check for clear tags (<div clear="..."/>)
    if (HtmlTagType.DIV.equals(tag.getType())) {
      String clearValue = getClearValue(tag);
      if (clearValue != null) {
        String clearReplacement = getClearReplacement(clearValue);
        if (clearReplacement != null) {
          errorResult.addReplacement(clearReplacement, false);
        }
      }
    }

    // Check for id tags (<span id="..."/> or <div id="..."/>)
    if ((tmpAnchorTemplates != null) &&
        !tmpAnchorTemplates.isEmpty()) {
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
        for (String[] anchorTemplate : tmpAnchorTemplates) {
          if ((anchorTemplate.length > 0) && (anchorTemplate[0].length() > 0)) {
            TemplateBuilder builder = TemplateBuilder.from(anchorTemplate[0]);
            builder.addParam(
                ((anchorTemplate.length > 1) && !"1".equals(anchorTemplate[1])) ? anchorTemplate[1] : null,
                idAttribute);
            errorResult.addReplacement(builder.toString());
          }
        }
      }
    }

    errorResult.addReplacement("");
    errors.add(errorResult);

    return true;
  }

  /**
   * Analyze a page to check for incorrectly written tags.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tagTypes Tag types.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeIncorrectTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      Set<TagType> tagTypes) {

    boolean result = false;

    // Check for incorrectly written tags (</xxx/>)
    int currentIndex = 0;
    String contents = analysis.getContents();
    while ((currentIndex >= 0) && (currentIndex < contents.length())) {
      currentIndex = contents.indexOf('<', currentIndex);
      String selectedTagName = null;
      if (currentIndex < 0) {
        return result;
      }
      if (currentIndex >= 0) {
        int beginIndex = currentIndex;
        boolean ok = true;
        currentIndex++;
        if (ok) {
          currentIndex = ContentsUtil.moveIndexAfterWhitespace(contents, currentIndex);
          if ((currentIndex < contents.length()) &&
              contents.charAt(currentIndex) == '/') {
            currentIndex++;
          } else {
            ok = false;
          }
        }
        if (ok &&
            (currentIndex < contents.length())) {
          currentIndex = ContentsUtil.moveIndexAfterWhitespace(contents, currentIndex);
        }
        if (ok) {
          for (TagType tagType : tagTypes) {
            int length = tagType.getNormalizedName().length();
            if ((selectedTagName == null) &&
                (currentIndex + length < contents.length()) &&
                ContentsUtil.startsWithIgnoreCase(contents, tagType.getNormalizedName(), currentIndex) &&
                !Character.isLetterOrDigit(contents.charAt(currentIndex + length))) {
              currentIndex += length;
              selectedTagName = tagType.getNormalizedName();
            }
          }
          if (selectedTagName == null) {
            ok = false;
          }
        }
        if (ok) {
          currentIndex = ContentsUtil.moveIndexAfterWhitespace(contents, currentIndex);
          if ((currentIndex < contents.length()) &&
              contents.charAt(currentIndex) == '/') {
            currentIndex++;
          } else {
            ok = false;
          }
        }
        if (ok) {
          currentIndex = ContentsUtil.moveIndexAfterWhitespace(contents, currentIndex);
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
          errorResult.addReplacement(
              TagBuilder.from(selectedTagName, TagFormat.CLOSE).toString());
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
   * @param tagType Tag type.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeSelfClosingTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      TagType tagType) {

    // Check for incorrect self closing tags
    String tagName = tagType.getNormalizedName();
    boolean result = false;
    int currentIndex = 0;
    String contents = analysis.getContents();
    int maxSize = contents.length();
    while (currentIndex < maxSize) {
      int nextIndex = currentIndex + 1;
      boolean shouldCheck = true;

      // Check if the current character can be the beginning of a tag
      if (shouldCheck && (contents.charAt(currentIndex) != '<')) {
        shouldCheck = false;
      }

      // Check if this is a self closing tag for the given name
      if (shouldCheck) {
        int tmpIndex = ContentsUtil.moveIndexAfterWhitespace(contents, currentIndex + 1);
        boolean incorrectChar = false;
        while ((tmpIndex < maxSize) &&
               (" \\.,:?/\n|+&)(".indexOf(contents.charAt(tmpIndex)) >= 0)) {
          tmpIndex++;
          incorrectChar = true;
        }
        boolean selfClosingTag = ContentsUtil.startsWithIgnoreCase(contents, tagName, tmpIndex);
        if (selfClosingTag) {
          tmpIndex += tagName.length();
        }
        if ((tmpIndex < maxSize) && selfClosingTag) {
          char tmpChar = contents.charAt(tmpIndex);
          selfClosingTag = !Character.isUpperCase(tmpChar) && !Character.isLowerCase(tmpChar);
        }
        if ((tmpIndex < maxSize) && selfClosingTag) {
          tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, tmpIndex, " \n");
          while ((tmpIndex < maxSize) &&
                 (" \\.,:?\n|+&)(`".indexOf(contents.charAt(tmpIndex)) >= 0)) {
            tmpIndex++;
            incorrectChar = true;
          }
          tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, tmpIndex, " \n");
          if ((tmpIndex < maxSize) && (contents.charAt(tmpIndex) == '/')) {
            tmpIndex++;
          }
          tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, tmpIndex, " \n");
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
                PageElementTag tag = analysis.isInTag(currentIndex, tagType);
                if (tag == null) {
                  shouldReport = true;
                }
              }
            }

            // Check if we are in a comment
            if (shouldReport) {
              ContentsComment comment = analysis.comments().getLargestAt(currentIndex);
              if (comment != null) {
                shouldReport = false;
                nextIndex = comment.getEndIndex();
              }
            }

            //
            if (shouldReport) {
              if (errors == null) {
                return true;
              }
              result = true;
              boolean endsWithGT = (contents.charAt(tmpIndex) == '>');
              if (endsWithGT) {
                tmpIndex++;
              }
              boolean automatic = endsWithGT && incorrectChar && analysis.getPage().isArticle();
              if (!automatic) {
                String text = contents.substring(currentIndex, tmpIndex);
                automatic |= erroneousTags.contains(text);
              }
              boolean close = analysis.getWPCConfiguration().getBoolean(
                  WPCConfigurationBoolean.CLOSE_SELF_CLOSING_TAGS);
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, currentIndex, tmpIndex);
              errorResult.addReplacement(
                  TagBuilder.from(tagName, false, close).toString(),
                  automatic);
              errors.add(errorResult);
              nextIndex = tmpIndex;
            }
          }
        }
      }

      currentIndex = nextIndex;
    }

    // Check for self closing tags with extra characters
    if (HtmlTagType.BR.equals(tagType)) {
      List<PageElementTag> tags = analysis.getTags(tagType);
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
              boolean automatic = !clearReplacement.isEmpty() && analysis.getPage().isArticle();
              errorResult.addReplacement(clearReplacement, automatic);
            }
          }
          if (extra || (tag.getParametersCount() > 0)) {
            errorResult.addReplacement(
                TagBuilder.from(tagName, TagFormat.FULL).toString(),
                false);
            errorResult.addReplacement(
                TagBuilder.from(tagName, TagFormat.OPEN).toString(),
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
    if ("all".equalsIgnoreCase(clearValue) || "both".equalsIgnoreCase(clearValue)) {
      return clearAll;
    }
    if ("left".equalsIgnoreCase(clearValue)) {
      return clearLeft;
    }
    if ("right".equalsIgnoreCase(clearValue)) {
      return clearRight;
    }
    return null;
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

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Replacements for anchors */
  private static final String PARAMETER_ANCHOR_TEMPLATES = "anchor_templates";

  /** Replacement for clear all */
  private static final String PARAMETER_CLEAR_ALL = "clear_all";

  /** Replacement for clear left */
  private static final String PARAMETER_CLEAR_LEFT = "clear_left";

  /** Replacement for clear right */
  private static final String PARAMETER_CLEAR_RIGHT = "clear_right";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_ANCHOR_TEMPLATES, true, true, false);
    anchorTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        anchorTemplates.addAll(tmpList);
      }
    } else {
      List<String[]> tmpList = getWPCConfiguration().getStringArrayList(
          WPCConfigurationStringList.ANCHOR_TEMPLATES);
      if (tmpList != null) {
        anchorTemplates.addAll(tmpList);
      }
    }
    clearAll = getSpecificProperty(PARAMETER_CLEAR_ALL, true, true, false);
    clearLeft = getSpecificProperty(PARAMETER_CLEAR_LEFT, true, true, false);
    clearRight = getSpecificProperty(PARAMETER_CLEAR_RIGHT, true, true, false);
  }

  /** Replacements for anchors */
  private final List<String[]> anchorTemplates = new ArrayList<>();

  /** Replacement for clear all */
  private String clearAll = null;

  /** Replacement for clear left */
  private String clearLeft = null;

  /** Replacement for clear right */
  private String clearRight = null;

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_ANCHOR_TEMPLATES,
        GT._T("A replacement for {0}", "&lt;span id=\"xxx\"/&gt;"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement("template name", GT._T("Name of the template")),
            new AlgorithmParameterElement("parameter name", GT._T("Name of the parameter"), true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_CLEAR_ALL,
        GT._T("A replacement for {0}", "&lt;br clear=\"all\"/&gt;"),
        new AlgorithmParameterElement(
            "replacement",
            GT._T("A replacement for {0}", "&lt;br clear=\"all\"/&gt;"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_CLEAR_LEFT,
        GT._T("A replacement for {0}", "&lt;br clear=\"left\"/&gt;"),
        new AlgorithmParameterElement(
            "replacement",
            GT._T("A replacement for {0}", "&lt;br clear=\"left\"/&gt;"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_CLEAR_RIGHT,
        GT._T("A replacement for {0}", "&lt;br clear=\"right\"/&gt;"),
        new AlgorithmParameterElement(
            "replacement",
            GT._T("A replacement for {0}", "&lt;br clear=\"right\"/&gt;"))));
  }
}
