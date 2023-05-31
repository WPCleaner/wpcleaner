/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a08x.a081;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTagRef;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;

class RefTagsCollector {

  Map<String, Map<String, List<PageElementTag>>> group(final PageAnalysis analysis) {
    List<PageElementTag> completeRefTags = analysis.getCompleteTags(WikiTagType.REF);
    if (completeRefTags.isEmpty()) {
      return Collections.emptyMap();
    }

    // Group references by group name and value
    final Map<String, Map<String, List<PageElementTag>>> result = new HashMap<>();
    String contents = analysis.getContents();
    for (PageElementTag tag : completeRefTags) {
      int valueBeginIndex = tag.getValueBeginIndex();
      int valueEndIndex = tag.getValueEndIndex();
      if ((!tag.isFullTag()) &&
          (valueBeginIndex > 0) &&
          (valueEndIndex > 0) &&
          (valueBeginIndex < valueEndIndex)) {

        // Retrieve references with the same group name
        String groupName = PageElementTagRef.getGroup(tag, analysis);
        Map<String, List<PageElementTag>> groupRefs = result.computeIfAbsent(groupName, name -> new HashMap<>());

        // Retrieve references with the same text
        String text = contents.substring(valueBeginIndex, valueEndIndex).trim();
        if (text.length() > 0) {
          groupRefs.computeIfAbsent(text, value -> new ArrayList<>()).add(tag);
        }
      }
    }

    return result;
  }
}
