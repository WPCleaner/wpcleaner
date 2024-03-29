/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a081;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTagRef;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsElement;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.CompleteTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.TagBuilder;
import org.wikipediacleaner.api.data.contents.tag.TagFormat;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.CompositeTextProvider;
import org.wikipediacleaner.utils.SimpleTextProvider;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringChecker.Result;
import org.wikipediacleaner.utils.StringCheckerReferenceName;
import org.wikipediacleaner.utils.TextProvider;
import org.wikipediacleaner.utils.TextProviderUrlTitle;


/**
 * Algorithm for analyzing error 81 of check wikipedia project.
 * Error 81: Reference duplication.
 */
public class CheckErrorAlgorithm081 extends CheckErrorAlgorithmBase {

  /**
   * String checker for the reference name.
   */
  private final StringChecker nameChecker;

  private final RefTagsCollector refTagsCollector;
  
  private final RefTagSelector refTagSelector;

  public CheckErrorAlgorithm081() {
    super("Reference duplication");
    nameChecker = new StringCheckerReferenceName();
    refTagsCollector = new RefTagsCollector();
    refTagSelector = new RefTagSelector();
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Group tags and check if we need to go further
    Map<String, Map<String, List<PageElementTag>>> refs = refTagsCollector.group(analysis);
    if (refs.values().stream().map(Map::values).flatMap(Collection::stream).map(List::size).noneMatch(size -> size > 1)) {
      return false;
    }

    // Analyze tags having the same group and value
    List<ContentsElement> referencesElements = new ArrayList<>();
    referencesElements.addAll(analysis.getTags(WikiTagType.REFERENCES));
    referenceTemplates.forEach(referencesTemplate -> referencesElements.addAll(analysis.getTemplates(referencesTemplate[0])));
    List<String> addedNames = new ArrayList<>();
    boolean result = false;
    for (Entry<String, Map<String, List<PageElementTag>>> entryGroup : refs.entrySet()) {
      String groupName = entryGroup.getKey();
      for (Entry<String, List<PageElementTag>> entryValue : entryGroup.getValue().entrySet()) {
        result |= analyzeTags(
            analysis,
            entryValue.getValue(),
            groupName,
            errors,
            addedNames,
            referencesElements);
      }
    }
    return result;
  }

  private boolean analyzeTags(
      PageAnalysis analysis,
      List<PageElementTag> tags,
      String groupName,
      Collection<CheckErrorResult> errors,
      List<String> addedNames,
      List<ContentsElement> referencesElements) {
    if (tags.size() <= 1) {
      return false;
    }

    // Handle references tag in between
    int minArea = tags.stream().mapToInt(PageElementTag::getCompleteEndIndex).min().orElse(Integer.MAX_VALUE);
    int maxArea = tags.stream().mapToInt(PageElementTag::getCompleteBeginIndex).max().orElse(0);
    for (ContentsElement referencesElement : referencesElements) {
      // TODO: Handle group name
      if (referencesElement.getBeginIndex() >= minArea && referencesElement.getEndIndex() <= maxArea) {
        return tags.stream()
            .collect(Collectors.groupingBy(tag -> tag.getCompleteEndIndex() < referencesElement.getEndIndex()))
            .values().stream()
            .map(tmpTags -> analyzeTags(analysis, tmpTags, groupName, errors, addedNames, referencesElements))
            .reduce(Boolean::logicalOr)
            .orElse(Boolean.FALSE);
      }
    }

    if (errors == null) {
      return true;
    }

    // Handle unnamed tags
    PageElementTag mainTag = refTagSelector.selectBestTag(tags, analysis);
    if (mainTag == null) {
      boolean firstTag = true;
      for (PageElementTag tag : tags) {
        reportUnnamedTag(analysis, tag, firstTag, groupName, errors, addedNames);
        firstTag = false;
      }
      return true;
    }

    // Handle named main tag
    tags.forEach(tag -> reportWithNamedTag(analysis, tag, groupName, mainTag, errors));

    return true;
  }

  private void reportWithNamedTag(
      PageAnalysis analysis,
      PageElementTag tag,
      String groupName,
      PageElementTag mainTag,
      Collection<CheckErrorResult> errors) {
    // Handle main tag
    if (tag == mainTag) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis,
          tag.getCompleteBeginIndex(), tag.getCompleteEndIndex(),
          CheckErrorResult.ErrorLevel.CORRECT);
      errors.add(errorResult);
      return;
    }

    // Handle other tags
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    String selectedName = PageElementTagRef.getName(mainTag);
    String nameValue = PageElementTagRef.getName(tag);
    boolean automatic = false;
    if (nameValue == null) {
      errorResult.addText(GT._T("Tag is unnamed"));
      automatic = true;
    } else if (selectedName.equals(nameValue)) {
      errorResult.addText(GT._T("Both tags have already the same name"));
      automatic = true;
    } else {
      errorResult.addText(GT._T("Tags have different names"));
      errorResult.addText(selectedName);
      errorResult.addText(nameValue);

      automatic = true;
      List<PageElementTag> tagsWithSameName = PageElementTagRef.getTagsWithName(nameValue, analysis);
      for (PageElementTag otherTag : tagsWithSameName) {
        String otherText = analysis.getContents().substring(otherTag.getValueBeginIndex(), otherTag.getValueEndIndex()).trim();
        if (otherText.isEmpty() && (otherTag != tag)) {
          automatic = false;
          CheckErrorResult otherErrorResult = createCheckErrorResult(
              analysis,
              otherTag.getCompleteBeginIndex(), otherTag.getCompleteEndIndex(),
              CheckErrorResult.ErrorLevel.WARNING);
          otherErrorResult.addText(GT._T("Tags have different names"));
          otherErrorResult.addText(selectedName);
          otherErrorResult.addText(nameValue);
          otherErrorResult.addReplacement(
              getClosedRefTag(groupName, selectedName, null),
              true);
          errors.add(otherErrorResult);
        }
      }
    }
    errorResult.addReplacement(
        getClosedRefTag(groupName, selectedName, null),
        automatic);
    errors.add(errorResult);
  }

  private void reportUnnamedTag(
      PageAnalysis analysis,
      PageElementTag tag,
      boolean firstTag,
      String groupName,
      Collection<CheckErrorResult> errors,
      List<String> addedNames) {

    int valueBeginIndex = tag.getValueBeginIndex();
    int valueEndIndex = tag.getValueEndIndex();

    // Find external links in the reference tag
    List<PageElementExternalLink> links = analysis.getExternalLinks().stream()
        .filter(link -> link.getBeginIndex() >= valueBeginIndex)
        .filter(link -> link.getEndIndex() <= valueEndIndex)
        .collect(Collectors.toList());

    // Find templates in the reference tag
    List<PageElementTemplate> templates = analysis.getTemplates().stream()
        .filter(template -> template.getBeginIndex() >= valueBeginIndex)
        .filter(template -> template.getEndIndex() <= valueEndIndex)
        .collect(Collectors.toList());

    // Register error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis,
        tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
    errorResult.addText("Both tags are unnamed");
    errors.add(errorResult);
    if (!firstTag) {
      return;
    }

    // Add an action for naming the reference tag
    String contents = analysis.getContents();
    List<TextProvider> providers = new ArrayList<>();
    links
        .stream()
        .map(link -> link.getLink())
        .map(link -> new TextProviderUrlTitle(link))
        .forEach(provider -> providers.add(provider));
    links.stream()
        .filter(link -> link.hasSquare())
        .filter(link -> link.getText() != null)
        .map(link -> link.getText())
        .map(text -> new SimpleTextProvider(text))
        .forEach(provider -> providers.add(provider));
    List<TextProvider> templateTextProviders = new ArrayList<>();
    for (PageElementTemplate template : templates) {
      TemplateConfiguration config = configurationByTemplateName.get(template.getTemplateName());
      if (config != null) {
        List<TextProvider> textProviders = config.getTextProviders(template);
        providers.addAll(textProviders);
        templateTextProviders.addAll(textProviders);
      }
    }
    Optional<String> optionalName = templateTextProviders.stream()
        .map(TextProvider::getTexts)
        .flatMap(Collection::stream)
        .filter(text -> (text != null) && !text.isEmpty())
        .map(this::cleanName)
        .filter(text -> text != null)
        .findFirst();
    if (optionalName.isPresent()) {
      final String suggestedName = optionalName.get();
      if (PageElementTagRef.getTagsWithName(suggestedName, analysis).isEmpty() &&
          !addedNames.contains(suggestedName)) {
        final String replacement =
            getOpenRefTag(groupName, suggestedName, null) +
            contents.substring(tag.getEndIndex(), tag.getCompleteEndIndex());
        errorResult.addReplacement(replacement, GT._T("Name the reference from template parameter"), automaticTitle);
        addedNames.add(suggestedName);
      }
    } else {
      final String suggestedName = generateName(analysis, addedNames);
      if (suggestedName != null) {
        final String replacement =
            getOpenRefTag(groupName, suggestedName, null) +
            contents.substring(tag.getEndIndex(), tag.getCompleteEndIndex());
        errorResult.addReplacement(replacement, GT._T("Generate a reference name"), automaticTitle);
        addedNames.add(suggestedName);
      }
    }
    String prefix = contents.substring(tag.getBeginIndex(), tag.getEndIndex() - 1);
    String suffix = contents.substring(tag.getEndIndex() - 1, tag.getCompleteEndIndex());
    errorResult.addPossibleAction(
        GT._T("Give a name to the <ref> tag"),
        new AddTextActionProvider(
            prefix + " name=\"",
            "\"" + suffix,
            new CompositeTextProvider(providers),
            GT._T("What name would you like to use for the <ref> tag ?"),
            nameChecker));

    // Add actions for external links
    for (PageElementExternalLink link : links) {
      errorResult.addPossibleAction(new SimpleAction(
          GT._T("External Viewer"),
          new ActionExternalViewer(link.getLink())));
    }
  }

  private String cleanName(final String name) {
    if (name == null) {
      return null;
    }
    Result result = nameChecker.checkString(name);
    if (result.isOk()) {
      if (name.length() > maxLength) {
        int newLength = ContentsUtil.moveIndexBackwardWhileNotFound(name, maxLength, " ");
        if (newLength > 5) {
          return name.substring(0, newLength);
        }
        newLength = ContentsUtil.moveIndexForwardWhileNotFound(name, maxLength, " ");
        if (newLength < name.length()) {
          return name.substring(0, newLength);
        }
      }
      return name;
    }
    if (!Objects.equals(name,  result.getText())) {
      return cleanName(result.getText());
    }
    return null;
  }

  private String generateName(PageAnalysis analysis, List<String> addedNames) {
    if (namePrefix.isEmpty() || Character.isDigit(namePrefix.charAt(0))) {
      return null;
    }
    int refNum = 1;
    while (true) {
      final String name = namePrefix + refNum;
      if (PageElementTagRef.getTagsWithName(name, analysis).isEmpty() &&
          !addedNames.contains(name)) {
        return name;
      }
      refNum++;
    }
  }

  /**
   * Construct an open reference tag.
   * 
   * @param groupName Name of the group.
   * @param tagName Name of the tag.
   * @param value Value of the tag.
   * @return Reference tag.
   */
  private String getOpenRefTag(String groupName, String tagName, String value) {
    TagBuilder builder = TagBuilder.from(WikiTagType.REF, TagFormat.OPEN);
    if ((groupName != null) && (groupName.trim().length() > 0)) {
      builder.addAttribute("group", groupName.trim());
    }
    if ((tagName != null) && (tagName.trim().length() > 0)) {
      builder.addAttribute("name", tagName.trim());
    }
    return builder.toString();
  }

  /**
   * Construct a closed reference tag.
   * 
   * @param groupName Name of the group.
   * @param tagName Name of the tag.
   * @param value Value of the tag.
   * @return Reference tag.
   */
  private String getClosedRefTag(String groupName, String tagName, String value) {
    CompleteTagBuilder builder = CompleteTagBuilder.from(WikiTagType.REF, StringUtils.trim(value));
    if ((groupName != null) && (groupName.trim().length() > 0)) {
      builder.addAttribute("group", groupName.trim());
    }
    if ((tagName != null) && (tagName.trim().length() > 0)) {
      builder.addAttribute("name", tagName.trim());
    }
    return builder.toString();
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if ((referenceTemplates == null) ||
        (referenceTemplates.isEmpty()) ||
        analysis.getPage().isInUserNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Template parameters for title  */
  private static final String PARAMETER_TEMPLATE_PARAMS = "template_params";

  /** Boolean telling if we can automatically use the title to generate the name */
  private static final String PARAMETER_AUTOMATIC_TITLE = "automatic_title";

  /** Prefix for reference names generated automatically */
  private static final String PARAMETER_NAME_PREFIX = "name_prefix";

  /** Max length for reference name (hint only) */
  private static final String PARAMETER_NAME_MAX_LENGTH = "name_max_length";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    List<String[]> refTemplates = getWPCConfiguration().getStringArrayList(
        WPCConfigurationStringList.REFERENCES_TEMPLATES);
    referenceTemplates.clear();
    if (refTemplates != null) {
      referenceTemplates.addAll(refTemplates);
    }

    TemplateConfigurationGroup group = new TemplateConfigurationGroup();
    List<String[]> generalList = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.TEMPLATE_GROUPS);
    if (generalList != null) {
      group.addGroups(generalList);
    }

    String tmp = getSpecificProperty(PARAMETER_TEMPLATE_PARAMS, true, true, false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.addParametersForTitle(tmpList, configurationByTemplateName, group);
    }

    tmp = getSpecificProperty(PARAMETER_AUTOMATIC_TITLE, true, true, false);
    automaticTitle = false;
    if (tmp != null) {
      automaticTitle = Boolean.valueOf(tmp);
    }

    tmp = getSpecificProperty(PARAMETER_NAME_PREFIX, true, true, false);
    namePrefix = StringUtils.defaultString(tmp, "");

    refTagSelector.setConfiguration(getWPCConfiguration());
  }
  
  /** Reference templates */
  private final List<String[]> referenceTemplates = new ArrayList<>();

  /** Flag to automatically use the title */
  private boolean automaticTitle = false;

  /** Prefix used for generating reference names */
  private String namePrefix = "";

  /** Hint for a maximum length of the reference name */
  private int maxLength = Integer.MAX_VALUE;

  /** Configuration by template */
  private final Map<String, TemplateConfiguration> configurationByTemplateName = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATE_PARAMS,
        GT._T("Parameters that can be used as title"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "param",
                GT._T("Name of the parameter"),
                true,
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_AUTOMATIC_TITLE,
        GT._T("Name the reference with the title"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "automatic",
                GT._T("Is it automatic?") + " (true/false)")
        },
        false));
    addParameter(new AlgorithmParameter(
        PARAMETER_NAME_PREFIX,
        GT._T("Prefix for the automatically generated reference names"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "prefix",
                GT._T("Prefix for the automatically generated reference names"))
        },
        false));
    addParameter(new AlgorithmParameter(
        PARAMETER_NAME_MAX_LENGTH,
        GT._T("Hint for a maximum length of the reference name"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "length",
                GT._T("Hint for a maximum length of the reference name"))
        },
        false));
  }
}
