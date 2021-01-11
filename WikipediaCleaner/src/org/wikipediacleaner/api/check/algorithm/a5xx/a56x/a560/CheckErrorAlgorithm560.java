/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a560;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 560 of check wikipedia project.
 * Error 560: tag with duplicated attributes.
 */
public class CheckErrorAlgorithm560 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm560() {
    super("tag with duplicated attributes");
  }

  /** Tags handled by this error : tag type -> attribute name -> automatic fixing */
  private static final Map<TagType, Map<String, Boolean>> TAGS_MAP;

  static {
    Map<TagType, Map<String, Boolean>> tagsMap = new HashMap<>();
    Map<String, Boolean> refTagMap = new HashMap<>();
    refTagMap.put("name", Boolean.TRUE);
    refTagMap.put("group", Boolean.TRUE);
    tagsMap.put(WikiTagType.REF, Collections.unmodifiableMap(refTagMap));
    TAGS_MAP = Collections.unmodifiableMap(tagsMap);
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

    // Analyze each type of tag
    boolean result = false;
    for (Entry<TagType, Map<String, Boolean>> tagConfiguration : TAGS_MAP.entrySet()) {
      List<PageElementTag> tags = analysis.getTags(tagConfiguration.getKey());
      for (PageElementTag tag : tags) {
        result |= analyzeTag(analysis, errors, tag, tagConfiguration.getValue());
      }
    }

    return result;
  }

  /**
   * Analyze a tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param tag Tag to be analyzed.
   * @param tagConfiguration Configuration for the tag.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeTag(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTag tag,
      Map<String, Boolean> tagConfiguration) {

    // Analyze only opening tags with several parameters
    if ((tag == null) || tag.isEndTag()) {
      return false;
    }
    if (tag.getParametersCount() < 2) {
      return false;
    }

    // Analyze each parameter name
    boolean result = false;
    for (Entry<String, Boolean> paramConfiguration : tagConfiguration.entrySet()) {
      boolean paramFound = false;
      for (int paramNum = tag.getParametersCount() - 1; paramNum >= 0; paramNum--) {
        Parameter param = tag.getParameter(paramNum);
        if ((param != null) &&
            StringUtils.equals(param.getName(), paramConfiguration.getKey())) {
          if (paramFound) {
            if (errors == null) {
              return true;
            }
            Parameter nextParam = tag.getParameter(paramNum + 1);
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                tag.getBeginIndex() + param.getOffsetBegin(),
                tag.getBeginIndex() + nextParam.getOffsetBegin());
            errorResult.addReplacement("", paramConfiguration.getValue());
            errors.add(errorResult);
          }
          paramFound = true;
        }
      }
    }

    return result;
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
}
