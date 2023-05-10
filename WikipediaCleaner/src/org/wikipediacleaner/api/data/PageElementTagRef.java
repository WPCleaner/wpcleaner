/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.List;
import java.util.stream.Collectors;

import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

public final class PageElementTagRef {

  private PageElementTagRef() {
    // Utility class
  }

  /**
   * Find the main reference tag in a list of reference tags.
   * 
   * @param refs List of reference tags.
   * @param references List of references tags.
   * @param analysis Page analysis.
   * @return Main reference tag in the list.
   */
  public static PageElementTag getMain(
      List<PageElementTag> refs,
      List<PageElementTag> references,
      PageAnalysis analysis) {
    if (refs == null) {
      return null;
    }

    // Configuration
    WPCConfiguration config = analysis.getWPCConfiguration();
    List<String[]> templates = config.getStringArrayList(WPCConfigurationStringList.REFERENCES_TEMPLATES);

    // Extract only named reference tags
    List<PageElementTag> namedAndValuedRefs = refs.stream()
        .filter(tag -> hasName(tag))
        .filter(tag -> hasValue(analysis, tag))
        .collect(Collectors.toList());
    if (namedAndValuedRefs.isEmpty()) {
      return null;
    }

    // Deal with named reference tag inside <references/>
    for (PageElementTag tag : namedAndValuedRefs) {
      for (PageElementTag reference : references) {
        if ((tag.getCompleteBeginIndex() > reference.getCompleteBeginIndex()) &&
            (tag.getCompleteEndIndex() < reference.getCompleteEndIndex())) {
          return tag;
        }
      }
    }
    
    // Deal with named reference tag inside template
    for (PageElementTag tag : namedAndValuedRefs) {
      if (templates != null) {
        PageElementTemplate template = analysis.isInTemplate(tag.getCompleteBeginIndex());
        if (template != null) {
          for (String[] elements : templates) {
            if ((elements.length > 0) &&
                (Page.areSameTitle(template.getTemplateName(), elements[0]))) {
              return tag;
            }
          }
        }
      }
    }

    // Return first named tag
    return namedAndValuedRefs.get(0);
  }

  private static boolean hasName(PageElementTag tag) {
    Parameter name = tag.getParameter("name");
    if ((name != null) &&
        (name.getTrimmedValue() != null) &&
        (!name.getTrimmedValue().isEmpty())) {
      return true;
    }
    return false;
  }

  private static boolean hasValue(PageAnalysis analysis, PageElementTag tag) {
    int beginValue = tag.getValueBeginIndex();
    int endValue = tag.getValueEndIndex();
    String value = analysis.getContents().substring(beginValue, endValue);
    if ((value != null) && (!value.trim().isEmpty())) {
      return true;
    }
    return false;
  }
}
