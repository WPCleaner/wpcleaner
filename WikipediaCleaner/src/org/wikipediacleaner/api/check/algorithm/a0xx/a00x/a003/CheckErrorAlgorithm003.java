/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a00x.a003;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.title.TitleBuilder;
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
    PageElementTag lastRefTag = null;
    List<PageElementTag> refTags = analysis.getTags(WikiTagType.REF);
    if ((refTags != null) && (refTags.size() > 0)) {
      for (int numTag = refTags.size() - 1; (numTag >= 0) && (lastRefTag == null); numTag--) {
        boolean usefulRef = true;
        PageElementTag refTag = refTags.get(numTag);
        if (analysis.getSurroundingTag(WikiTagType.NOWIKI, refTag.getBeginIndex()) != null) {
          usefulRef =  false;
        }
        if (usefulRef) {
          lastRefTag = refTag;
        }
      }
    }
    if (lastRefTag == null) {
      return false;
    }
    final int lastRefTagIndex = lastRefTag.getCompleteEndIndex();

    // Analyzing text for <references> tags
    boolean referencesFound = false;
    List<PageElementTag> referencesTags = analysis.getTags(WikiTagType.REFERENCES);
    if (referencesTags != null) {
      for (PageElementTag referencesTag : referencesTags) {
        if (referencesTag.isComplete()) {
          return false;
        }
        referencesFound = true;
      }
    }

    // Search for templates like {{References}}
    if ((referencesTemplates != null) && !referencesTemplates.isEmpty()) {
      List<PageElementTemplate> allTemplates = analysis.getTemplates();
      int templateNum = allTemplates.size();
      while (templateNum > 0) {
        templateNum--;
        PageElementTemplate template = allTemplates.get(templateNum);
        if (referencesTemplates.contains(Page.normalizeTitle(template.getTemplateName()))) {
          return false;
        }
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        lastRefTag.getCompleteBeginIndex(),
        lastRefTag.getCompleteEndIndex(),
        referencesFound ? ErrorLevel.WARNING : ErrorLevel.ERROR);
    errors.add(errorResult);

    // Suggestion to close tag if references tags are unclosed
    String contents = analysis.getContents();
    if (referencesTags != null) {
      for (PageElementTag referencesTag : referencesTags) {
        if (!referencesTag.isComplete()) {
          errorResult = createCheckErrorResult(
              analysis, referencesTag.getBeginIndex(), referencesTag.getEndIndex());
          if (referencesTags.size() == 1) {
            errorResult.addReplacement(
                WikiTagType.REFERENCES.getFullTag(),
                GT._T("Close tag"));
          }
          errors.add(errorResult);
        }
      }
    }

    // Suggestion when there's one references tag
    if ((referencesTags != null) && (referencesTags.size() == 1)) {
      int index = referencesTags.get(0).getEndIndex();
      boolean ok = true;
      while (ok && (index < contents.length())) {
        char currentChar = contents.charAt(index);
        if (Character.isWhitespace(currentChar)) {
          index++;
        } else if (currentChar == '<') {
          PageElementTag tag = analysis.isInTag(index);
          if ((tag != null) &&
              (tag.getBeginIndex() == index) &&
              (WikiTagType.REF.equals(tag.getType()))) {
            index = tag.getCompleteEndIndex();
          } else {
            if (contents.startsWith("</references/>", index)) {
              errorResult = createCheckErrorResult(analysis, index, index + 14);
              errorResult.addReplacement(
                  WikiTagType.REFERENCES.getCloseTag(),
                  true);
              errors.add(errorResult);
            }
            ok = false;
          }
        } else {
          ok = false;
        }
      }
    }

    if ((referencesTags != null) && !referencesTags.isEmpty()) {
      return true;
    }
    if (insert == null) {
      return true;
    }
    final boolean canBeAutomatic = analysis.getPage().isInMainNamespace();

    // Search for titles where references tag can be added
    List<PageElementTitle> correctTitles = analysis.getTitles().stream()
        .filter(title -> isPossibleTitle(title))
        .collect(Collectors.toList());
    correctTitles.forEach(title -> {
      int beginIndex = title.getBeginIndex();
      int endIndex = title.getEndIndex();
      CheckErrorResult tmpErrorResult = createCheckErrorResult(
          analysis, beginIndex, endIndex, ErrorLevel.WARNING);
      String replacement = contents.substring(beginIndex, endIndex) + "\n" + insert;
      tmpErrorResult.addReplacement(
          replacement,
          (errors.size() == 1) && (lastRefTagIndex <= beginIndex) && canBeAutomatic);
      errors.add(tmpErrorResult);
    });

    // Search for titles before which references tag can be added
    List<PageElementTitle> titlesBefore = analysis.getTitles().stream()
        .filter(title -> isBeforeTitle(title))
        .collect(Collectors.toList());
    titlesBefore.forEach(title -> {
      addSuggestionBeforeElement(
          analysis, errors, title, title.getLevel(),
          (errors.size() == 1) && (lastRefTagIndex <= title.getBeginIndex()) && canBeAutomatic);
    });

    // Search for templates before which references tag can be added
    analysis.getTemplates().stream()
        .filter(template -> isBeforeTemplate(template))
        .findFirst()
        .ifPresent(template -> {
          int beginIndex = template.getBeginIndex();
          boolean automatic = (errors.size() == 1) && (lastRefTagIndex <= beginIndex);
          if (automatic) {
            int tmpIndex = ContentsUtil.moveIndexBackwardWhileFound(contents, beginIndex - 1, " ");
            automatic &= (contents.charAt(tmpIndex) != '}'); 
          }
          addSuggestionBeforeElement(analysis, errors, template, 2, automatic && canBeAutomatic);
        });

    // Fallback on adding the references tag before the categories
    if (errors.size() == 1) {
      List<PageElementCategory> categories = analysis.getCategories();
      if (!categories.isEmpty()) {
        ContentsElement firstCategory = categories.get(0);
        List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
        if (!defaultSorts.isEmpty() &&
            (defaultSorts.get(0).getBeginIndex() < firstCategory.getBeginIndex())) {
          firstCategory = defaultSorts.get(0);
        }
        addSuggestionBeforeElement(analysis, errors, firstCategory, 2, false);
      }
    }

    return true;
  }

  private void addSuggestionBeforeElement(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      ContentsElement element,
      int titleLevel,
      boolean automatic) {
    int beginIndex = element.getBeginIndex();
    int endIndex = element.getEndIndex();
    boolean needNewLineBefore = beginIndex > 0 && analysis.getContents().charAt(beginIndex - 1) != '\n';
    CheckErrorResult tmpErrorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex, ErrorLevel.WARNING);
    String replacement =
        (needNewLineBefore ? "\n\n" : "") +
        TitleBuilder.from(titleLevel, preferredTitle).toString() +
        "\n" + insert + "\n\n" +
        analysis.getContents().substring(beginIndex, endIndex);
    tmpErrorResult.addReplacement(replacement, automatic);
    errors.add(tmpErrorResult);
  }

  private boolean isPossibleTitle(final PageElementTitle title) {
    String titleText = title.getTitle();
    return (titleText != null) && titles.contains(titleText.toUpperCase());
  }

  private boolean isBeforeTitle(final PageElementTitle title) {
    String titleText = title.getTitle();
    return (titleText != null) && beforeTitles.contains(titleText.toUpperCase());
  }

  private boolean isBeforeTemplate(final PageElementTemplate template) {
    String templateName = template.getTemplateName();
    return (templateName != null) && beforeTemplates.contains(templateName);
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

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Text to insert for references */
  private static final String PARAMETER_INSERT = "insert";

  /** List of templates including references tag */
  private static final String PARAMETER_REFERENCES_TEMPLATES = "references_templates";

  /** List of templates including references tag */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Section titles where references can be inserted */
  private static final String PARAMETER_TITLES = "titles";

  /** Section titles where references can be inserted before */
  private static final String PARAMETER_BEFORE_TITLES = "before_titles";

  /** Templates where references can be inserted before */
  private static final String PARAMETER_BEFORE_TEMPLATES = "before_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    if (tmp == null) {
      tmp = getSpecificProperty(PARAMETER_REFERENCES_TEMPLATES, true, true, false);
    }
    referencesTemplates.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String tmpElement : tmpList) {
          referencesTemplates.add(Page.normalizeTitle(tmpElement));
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_INSERT, true, true, false);
    insert = null;
    if ((tmp != null) && !tmp.trim().isEmpty()) {
      insert = tmp.trim();
    }

    tmp = getSpecificProperty(PARAMETER_TITLES, true, true, false);
    titles.clear();
    preferredTitle = "";
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        tmpList.forEach(title -> titles.add(title.toUpperCase()));
        if (!tmpList.isEmpty()) {
          preferredTitle = tmpList.get(0);
        }
      }
    }

    tmp = getSpecificProperty(PARAMETER_BEFORE_TITLES, true, true, false);
    beforeTitles.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        tmpList.forEach(title -> beforeTitles.add(title.toUpperCase()));
      }
    }

    tmp = getSpecificProperty(PARAMETER_BEFORE_TEMPLATES, true, true, false);
    beforeTemplates.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        tmpList.forEach(template -> beforeTemplates.add(Page.normalizeTitle(template)));
      }
    }
  }

  /** List of templates including references tag */
  private final Set<String> referencesTemplates = new HashSet<>();

  /** Text to insert for references */
  private String insert = null;

  /** Section titles where references can be inserted */
  private final List<String> titles = new ArrayList<>();

  /** Preferred section title where references can be inserted */
  private String preferredTitle = "";

  /** Section titles where references can be inserted before */
  private final Set<String> beforeTitles = new HashSet<>();

  /** Templates where references can be inserted before */
  private final Set<String> beforeTemplates = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_INSERT,
        GT._T("Text to insert for adding {0}", "&lt;references/&gt;"),
        new AlgorithmParameterElement(
            "text",
            GT._T("Text to insert for adding {0}", "&lt;references/&gt;"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_REFERENCES_TEMPLATES,
        GT._T("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template resulting in the inclusion of {0}", "&lt;references/&gt;")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template resulting in the inclusion of {0}", "&lt;references/&gt;")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TITLES,
        GT._T("Section headings where a {0} can be added", "&lt;references/&gt;"),
        new AlgorithmParameterElement(
            "section heading",
            GT._T("Section heading where a {0} can be added", "&lt;references/&gt;")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_BEFORE_TITLES,
        GT._T("Section headings where a {0} can be added before", "&lt;references/&gt;"),
        new AlgorithmParameterElement(
            "section heading",
            GT._T("Section heading where a {0} can be added before", "&lt;references/&gt;")),
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_BEFORE_TEMPLATES,
        GT._T("Templates where a {0} can be added before", "&lt;references/&gt;"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template where a {0} can be added before", "&lt;references/&gt;")),
        true));
  }
}
