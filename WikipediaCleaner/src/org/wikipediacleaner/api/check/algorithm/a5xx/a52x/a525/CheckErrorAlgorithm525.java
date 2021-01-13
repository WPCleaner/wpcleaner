/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a52x.a525;

import java.awt.ComponentOrientation;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 525 of check wikipedia project.
 * Error 525: Useless span tag
 */
public class CheckErrorAlgorithm525 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm525() {
    super("Useless span tag");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyze each tag
    List<PageElementTag> tags = analysis.getCompleteTags(HtmlTagType.SPAN);
    if ((tags == null) || tags.isEmpty()) {
      return false;
    }
    boolean result = false;
    String contents = analysis.getContents();
    int lastIndex = 0;
    for (PageElementTag tag : tags) {
      // Decide if tag is useful
      boolean isUseless = true;
      boolean onlyUselessParameter = true;
      ErrorLevel level = ErrorLevel.ERROR;
      String idParam = null;
      for (int numParam = 0; numParam < tag.getParametersCount(); numParam++) {
        Parameter param = tag.getParameter(numParam);
        boolean isParameterUseless = false;
        String value = param.getTrimmedValue();
        if ((value != null) && (!value.isEmpty())) {
          String lang = analysis.getWikipedia().getSettings().getLanguage();
          if ("lang".equals(param.getName()) && (lang != null) && lang.equalsIgnoreCase(value)) {
            // useful
          } else if ("dir".equals(param.getName())) {
            ComponentOrientation dir = analysis.getWikipedia().getSettings().getComponentOrientation();
            if (("ltr".equalsIgnoreCase(value) && (dir == ComponentOrientation.LEFT_TO_RIGHT)) ||
                ("rtl".equalsIgnoreCase(value) && (dir == ComponentOrientation.RIGHT_TO_LEFT))) {
              // useful
            } else {
              isUseless = false;
            }
          } else if ("class".equals(param.getName()) && "cx-segment".equals(param.getValue())) {
            // useless: Content Translation tool garbage
            isParameterUseless = true;
          } else if ("data-segmentid".equals(param.getName())) {
            // useless: Content Translation tool garbage
            isParameterUseless = true;
          } else if ("contenteditable".equals(param.getName())) {
            // useless: Content Translation tool garbage
            isParameterUseless = true;
          } else if ("id".equals(param.getName())) {
            level = ErrorLevel.WARNING;
            idParam = param.getTrimmedValue();
          } else if ("class".equals(param.getName())) {
            level = ErrorLevel.WARNING;
          } else {
            isUseless = false;
          }
        }
        if (!isParameterUseless) {
          onlyUselessParameter = false;
        }
      }
      if (!tag.isComplete()) {
        isUseless = true;
      }

      if (isUseless && (tag.getBeginIndex() >= lastIndex)) {
        if (errors == null) {
          return true;
        }
        result = true;
        lastIndex = tag.getCompleteEndIndex();

        // Create error
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex(), level);
        if (tag.isFullTag() || !tag.isComplete()) {
          errorResult.addReplacement("");
        } else {

          String internal = contents.substring(
              tag.getValueBeginIndex(), tag.getValueEndIndex());

          // Suggestion for anchors
          if ((idParam != null) && !idParam.isEmpty()) {
            if (anchorTemplates.isEmpty()) {
              errorResult.addText(GT._T("Use an anchor template?"));
            }
            for (String[] anchorTemplate : anchorTemplates) {
              if ((anchorTemplate.length > 0) && (anchorTemplate[0].length() > 0)) {
                StringBuilder replacement = new StringBuilder();
                TemplateBuilder builder = TemplateBuilder.from(anchorTemplate[0]);
                builder.addParam(
                    ((anchorTemplate.length > 1) && !"1".equals(anchorTemplate[1])) ? anchorTemplate[1] : null,
                    idParam);
                replacement.append(builder.toString());
                replacement.append(internal);
                boolean automatic =
                    (tag.getParametersCount() == 1) &&
                    (anchorTemplate.length > 1) &&
                    Boolean.parseBoolean(anchorTemplate[2]) &&
                    !idParam.startsWith("mw") &&
                    !idParam.startsWith("cite");
                errorResult.addReplacement(replacement.toString(), automatic);
              }
            }
          }

          // Suggestion to remove the tag
          PageElementTag refTag = analysis.isInTag(
              tag.getCompleteEndIndex(), WikiTagType.REF);
          if ((refTag != null) && (refTag.isEndTag())) {
            internal = internal.trim();
          }
          errorResult.addReplacement(
              internal,
              GT._T("Remove {0} tags", HtmlTagType.SPAN.getOpenTag()),
              onlyUselessParameter);
        }
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Replacements for anchors */
  private static final String PARAMETER_ANCHOR_TEMPLATES = "anchor_templates";

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
  }

  /** Replacements for anchors */
  private final List<String[]> anchorTemplates = new ArrayList<>();

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
            new AlgorithmParameterElement(
                "template name",
                GT._T("A template that can replace a {0}", "&lt;span id=\"xxx\"/&gt;")),
            new AlgorithmParameterElement(
                "parameter name",
                GT._T("Name of the parameter to be used for the anchor identifier"),
                true),
            new AlgorithmParameterElement(
                "true/false",
                GT._T("If replacement can be automatic"),
                true)
        },
        true));
  }
}
