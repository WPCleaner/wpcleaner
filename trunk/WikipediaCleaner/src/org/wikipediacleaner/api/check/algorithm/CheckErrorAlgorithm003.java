/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 3 of check wikipedia project.
 * Error 3: Article with &lt;ref&gt; and no &lt;references /&gt;
 */
public class CheckErrorAlgorithm003 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm003() {
    super("Article with <ref> and no <references />");
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

    // Analyzing text for <ref> tags
    boolean refFound = false;
    List<PageElementTag> refTags = analysis.getTags(PageElementTag.TAG_WIKI_REF);
    if ((refTags != null) && (refTags.size() > 0)) {
      Iterator<PageElementTag> itRefTags = refTags.iterator();
      while (!refFound && itRefTags.hasNext()) {
        boolean usefulRef = true;
        PageElementTag refTag = itRefTags.next();
        if (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, refTag.getBeginIndex()) != null) {
          usefulRef =  false;
        }
        if (usefulRef) {
          refFound = true;
        }
      }
    }
    if (!refFound) {
      return false;
    }

    // Analyzing text for <references> tags
    List<PageElementTag> referencesTags = analysis.getTags(PageElementTag.TAG_WIKI_REFERENCES);
    if (referencesTags != null) {
      for (PageElementTag referencesTag : referencesTags) {
        if (referencesTag.isComplete()) {
          return false;
        }
      }
    }

    // Search for templates like {{References}}
    String templates = getSpecificProperty(
        "templates", true, true, false);
    if (templates == null) {
      templates = getSpecificProperty(
          "references_templates", true, true, false);
    }
    List<String> referencesTemplates = null;
    if (templates != null) {
      referencesTemplates = WPCConfiguration.convertPropertyToStringList(templates);
    }
    if (referencesTemplates != null) {
      List<PageElementTemplate> allTemplates = analysis.getTemplates();
      int templateNum = allTemplates.size();
      while (templateNum > 0) {
        templateNum--;
        PageElementTemplate template = allTemplates.get(templateNum);
        for (String referencesTemplate : referencesTemplates) {
          if (Page.areSameTitle(template.getTemplateName(), referencesTemplate)) {
            return false;
          }
        }
      }
    }

    // Try to make some suggestions
    if (errors == null) {
      return true;
    }
    String contents = analysis.getContents();
    if (referencesTags != null) {
      for (PageElementTag referencesTag : referencesTags) {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, referencesTag.getBeginIndex(), referencesTag.getEndIndex());
        if (referencesTags.size() == 1) {
          errorResult.addReplacement(
              PageElementTag.createTag(PageElementTag.TAG_WIKI_REFERENCES, true, true),
              GT._("Close tag"));
        }
        errors.add(errorResult);
        if (referencesTags.size() == 1) {
          int index = referencesTag.getEndIndex();
          boolean ok = true;
          while (ok && (index < contents.length())) {
            char currentChar = contents.charAt(index);
            if (Character.isWhitespace(currentChar)) {
              index++;
            } else if (currentChar == '<') {
              PageElementTag tag = analysis.isInTag(index);
              if ((tag != null) &&
                  (tag.getBeginIndex() == index) &&
                  (PageElementTag.TAG_WIKI_REF.equals(tag.getNormalizedName()))) {
                index = tag.getCompleteEndIndex();
              } else {
                if (contents.startsWith("</references/>", index)) {
                  errorResult = createCheckErrorResult(analysis, index, index + 14);
                  errorResult.addReplacement(PageElementTag.createTag(
                      PageElementTag.TAG_WIKI_REFERENCES, true, false), true);
                  errors.add(errorResult);
                }
                ok = false;
              }
            } else {
              ok = false;
            }
          }
        }
      }
    }

    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("references_templates", GT._("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"));
    parameters.put("templates", GT._("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"));
    return parameters;
  }
}
