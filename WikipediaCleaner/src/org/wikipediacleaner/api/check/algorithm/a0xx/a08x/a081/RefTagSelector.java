/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a081;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTagRef;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;

class RefTagSelector {

  private final Set<String> referencesTemplates;

  public RefTagSelector() {
    referencesTemplates = new HashSet<>();
  }

  public void setConfiguration(final WPCConfiguration config) {
    referencesTemplates.clear();
    List<String[]> templates = config.getStringArrayList(WPCConfigurationStringList.REFERENCES_TEMPLATES);
    if (templates != null) {
      for (String[] template : templates) {
        if (template.length > 0) {
          referencesTemplates.add(Page.normalizeTitle(template[0]));
        }
      }
    }
  }

  @Nullable PageElementTag selectBestTag(
      final List<PageElementTag> tags,
      final PageAnalysis analysis) {
    if (tags.size() <= 1) {
      return null;
    }

    // Keep only tags with a name and a value
    List<PageElementTag> selectedTags = tags.stream()
        .filter(this::hasName)
        .filter(tag -> hasValue(analysis, tag))
        .collect(Collectors.toList());
    if (selectedTags.isEmpty()) {
      return null;
    }
    if (selectedTags.size() == 1) {
      return selectedTags.get(0);
    }

    // Prefer a tag inside a <references />
    final List<PageElementTag> references = analysis.getCompleteTags(WikiTagType.REFERENCES);
    if (!references.isEmpty()) {
      for (PageElementTag tag : selectedTags) {
        for (PageElementTag reference : references) {
          if ((tag.getCompleteBeginIndex() > reference.getCompleteBeginIndex()) &&
              (tag.getCompleteEndIndex() < reference.getCompleteEndIndex())) {
            return tag;
          }
        }
      }
    }

    // Prefer a tag inside a references template
    if (!referencesTemplates.isEmpty()) {
      for (PageElementTag tag : selectedTags) {
        PageElementTemplate template = analysis.isInTemplate(tag.getCompleteBeginIndex());
        if ((template != null) && referencesTemplates.contains(template.getTemplateName())) {
          return tag;
        }
      }
    }

    // Filter tags in a template
    List<PageElementTag> tagsNotInTemplate = selectedTags.stream()
        .filter(tag -> analysis.isInTemplate(tag.getBeginIndex()) == null)
        .collect(Collectors.toList());
    if (!tagsNotInTemplate.isEmpty()) {
      selectedTags = tagsNotInTemplate;
    }

    // Filter name starting with ":" followed by digits
    List<PageElementTag> tagsWithRealName = selectedTags.stream()
        .filter(this::hasRealName)
        .collect(Collectors.toList());
    if (!tagsWithRealName.isEmpty()) {
      selectedTags = tagsWithRealName;
    }

    // Filter name equal to another name with number after
    final List<PageElementTag> compareTags = selectedTags;
    List<PageElementTag> tagsWithLowerName = selectedTags.stream()
        .filter(tag -> hasNoLowerName(tag, compareTags))
        .collect(Collectors.toList());
    if (!tagsWithLowerName.isEmpty()) {
      selectedTags = tagsWithLowerName;
    }

    // Return first named tag
    return selectedTags.get(0);
  }

  private boolean hasName(PageElementTag tag) {
    return PageElementTagRef.getName(tag) != null;
  }

  private boolean hasRealName(PageElementTag tag) {
    final String name = PageElementTagRef.getName(tag);
    if (name == null) {
      return false;
    }
    if (!name.startsWith(":")) {
      return true;
    }
    if (name.length() < 2) {
      return false;
    }
    if (ContentsUtil.moveIndexForwardWhileFound(name, 1, "0123456789") == name.length()) {
      return false;
    }
    return true;
  }

  private boolean hasNoLowerName(PageElementTag tag, List<PageElementTag> tags) {
    final String name = PageElementTagRef.getName(tag);
    if (name == null) {
      return false;
    }
    return tags.stream()
        .map(PageElementTagRef::getName)
        .filter(testName -> !name.equals(testName))
        .noneMatch(testName -> isLowerName(name, testName));
  }

  private boolean isLowerName(String name, String compareName) {
    if (!name.startsWith(compareName)) {
      return false;
    }
    if (name.length() <= compareName.length()) {
      return false;
    }
    return ContentsUtil.moveIndexForwardWhileFound(name, compareName.length(), "0123456789") == name.length();
  }

  private boolean hasValue(PageAnalysis analysis, PageElementTag tag) {
    int beginValue = tag.getValueBeginIndex();
    int endValue = tag.getValueEndIndex();
    String value = analysis.getContents().substring(beginValue, endValue);
    if ((value != null) && (!value.trim().isEmpty())) {
      return true;
    }
    return false;
  }
}
