/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a50x.a504;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 504 of check wikipedia project.
 * Error 504: Reference in title
 */
public class CheckErrorAlgorithm504 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm504() {
    super("Reference in title");
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

    // Check every reference
    Collection<PageElementTag> refs = analysis.getCompleteTags(WikiTagType.REF);
    if ((refs == null) || (refs.isEmpty())) {
      return false;
    }
    final Map<PageElementTitle, List<PageElementTag>> titles = refs.stream()
        .filter(ref -> analysis.isInTitle(ref.getCompleteBeginIndex()) != null)
        .collect(Collectors.groupingBy(ref -> analysis.isInTitle(ref.getCompleteBeginIndex())))
;
    if (titles.isEmpty()) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }

    titles.entrySet().forEach(entry -> reportTitle(analysis, entry.getKey(), entry.getValue(), errors));
    return true;
  }

  private void reportTitle(
      PageAnalysis analysis,
      PageElementTitle title,
      List<PageElementTag> refs,
      Collection<CheckErrorResult> errors) {
    CheckErrorResult error = createCheckErrorResult(analysis, title.getBeginIndex(), title.getEndIndex());
    String contents = analysis.getContents();

    // Construct title without references
    final PageElementTag firstRef = refs.get(0);
    final PageElementTag lastRef = refs.get(refs.size() - 1);
    String titleWithoutReferences =
        contents.substring(title.getBeginIndex(), firstRef.getCompleteBeginIndex()) +
        contents.substring(lastRef.getCompleteEndIndex(), title.getEndIndex());

    // Suggestion to move references in a template
    for (TemplateConfiguration template : templates) {
      String references = contents.substring(firstRef.getCompleteBeginIndex(), lastRef.getCompleteEndIndex());
      String replacement =
          titleWithoutReferences + "\n" +
          TemplateBuilder.from(template.templateName)
              .addParam(
                  template.paramName,
                  ((template.commentBefore != null) ? CommentBuilder.from(template.commentBefore).toString() + " " : "") + references)
              .removeUnnecessaryParamNames(true)
              .toString();
      String text = GT._T("Use template {0}", template.templateName);
      error.addReplacement(replacement, text, template.automatic);
    }

    // Suggestion to remove references
    error.addReplacement(titleWithoutReferences, GT._T("Remove references"));

    errors.add(error);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Parameter for templates*/
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    templates.clear();
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.parseConfiguration(tmpList, templates);
    }
  }
  
  /** Templates */
  private final List<TemplateConfiguration> templates = new ArrayList<>();
  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates that can be used to declare the reference for the entire section"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter")),
            new AlgorithmParameterElement(
                "automatic",
                "true/false",
                true),
            new AlgorithmParameterElement(
                "comment",
                GT._T("Comment about the template"),
                true)
        },
        true));
  }
}
