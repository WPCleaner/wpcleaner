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
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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
    List<PageElementTag> namedAndValuedTags = tags.stream()
        .filter(this::hasName)
        .filter(tag -> hasValue(analysis, tag))
        .collect(Collectors.toList());
    if (namedAndValuedTags.isEmpty()) {
      return null;
    }
    if (namedAndValuedTags.size() == 1) {
      return namedAndValuedTags.get(0);
    }

    // Prefer a named tag inside a <references />
    final List<PageElementTag> references = analysis.getCompleteTags(WikiTagType.REFERENCES);
    if (!references.isEmpty()) {
      for (PageElementTag tag : namedAndValuedTags) {
        for (PageElementTag reference : references) {
          if ((tag.getCompleteBeginIndex() > reference.getCompleteBeginIndex()) &&
              (tag.getCompleteEndIndex() < reference.getCompleteEndIndex())) {
            return tag;
          }
        }
      }
    }

    // Prefer a named tag inside a references template
    if (!referencesTemplates.isEmpty()) {
      for (PageElementTag tag : namedAndValuedTags) {
        PageElementTemplate template = analysis.isInTemplate(tag.getCompleteBeginIndex());
        if ((template != null) && referencesTemplates.contains(template.getTemplateName())) {
          return tag;
        }
      }
    }

    // TODO: better heuristics (like based on name to handle extra 2 added by VE)

    // Return first named tag
    return namedAndValuedTags.get(0);
  }

  private boolean hasName(PageElementTag tag) {
    Parameter name = tag.getParameter("name");
    if ((name != null) &&
        (name.getTrimmedValue() != null) &&
        (!name.getTrimmedValue().isEmpty())) {
      return true;
    }
    return false;
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
