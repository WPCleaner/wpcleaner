/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementMagicWord;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.contents.IntervalComparator;
import org.wikipediacleaner.i18n.GT;


/**
 * Base class for errors on table of contents.
 */
public abstract class CheckErrorAlgorithmTOC extends CheckErrorAlgorithmBase {

  /**
   * @param name Algorithm name.
   */
  public CheckErrorAlgorithmTOC(String name) {
    super(name);
  }

  /**
   * @param analysis Page analysis.
   * @param beginIndex Begin index.
   * @param endIndex End index.
   * @return List of elements creating a table of contents.
   */
  protected List<PageElement> getToCs(
      PageAnalysis analysis, int beginIndex, int endIndex) {
    List<PageElement> result = new ArrayList<PageElement>();

    // Magic words
    List<PageElementMagicWord> magicWords = analysis.getMagicWords();
    if (magicWords != null) {
      for (PageElementMagicWord magicWord : magicWords) {
        if ((magicWord.getBeginIndex() >= beginIndex) &&
            (magicWord.getEndIndex() <= endIndex) &&
            MagicWord.TOC.equals(magicWord.getMagicWord().getName())) {
          result.add(magicWord);
        }
      }
    }

    // Templates
    String templatesProp = getSpecificProperty("templates", true, true, false);
    if (templatesProp != null) {
      List<String> tmpTocTemplates = WPCConfiguration.convertPropertyToStringList(templatesProp);
      if (tmpTocTemplates != null) {
        List<String> tocTemplates = new ArrayList<String>();
        for (String tocTemplate : tmpTocTemplates) {
          tocTemplate = tocTemplate.toLowerCase();
          tocTemplate = tocTemplate.replaceAll("\\[ \\]\\+", " ");
          tocTemplates.add(tocTemplate);
        }
        List<PageElementTemplate> templates = analysis.getTemplates();
        for (PageElementTemplate template : templates) {
          if ((template.getBeginIndex() >= beginIndex) &&
              (template.getEndIndex() <= endIndex)) {
            boolean shouldAdd = false;
            String templateName = template.getTemplateName().toLowerCase();
            for (String tocTemplate : tocTemplates) {
              if (Page.areSameTitle(templateName, tocTemplate)) {
                shouldAdd = true;
              }
            }
            if (shouldAdd) {
              result.add(template);
            }
          }
        }
      }
    }

    Collections.sort(result, new IntervalComparator());
    return result;
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("templates", GT._("A list of templates resulting in the inclusion of a table of contents"));
    return parameters;
  }
}
