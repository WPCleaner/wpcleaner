/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
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
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Analyzing text for <ref> tags
    boolean refFound = false;
    List<PageElementTag> refTags = pageAnalysis.getTags(PageElementTag.TAG_WIKI_REF);
    if ((refTags != null) && (refTags.size() > 0)) {
      Iterator<PageElementTag> itRefTags = refTags.iterator();
      while (!refFound && itRefTags.hasNext()) {
        boolean usefulRef = true;
        PageElementTag refTag = itRefTags.next();
        if (pageAnalysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, refTag.getBeginIndex()) != null) {
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

    // Search for templates like {{References}}
    String templates = getSpecificProperty(
        "references_templates", true, true, false);
    List<String> referencesTemplates = null;
    if (templates != null) {
      referencesTemplates = WPCConfiguration.convertPropertyToStringList(templates);
    }
    if (referencesTemplates != null) {
      for (String referencesTemplate : referencesTemplates) {
        Collection<PageElementTemplate> foundTemplates = pageAnalysis.getTemplates(referencesTemplate);
        if ((foundTemplates != null) && (foundTemplates.size() > 0)) {
          return false;
        }
      }
    }

    // Analyzing text for <references> tags
    List<PageElementTag> referencesTags = pageAnalysis.getTags(PageElementTag.TAG_WIKI_REFERENCES);
    if ((referencesTags == null) || (referencesTags.isEmpty())) {
      return true;
    }
    for (PageElementTag referencesTag : referencesTags) {
      if (referencesTag.isComplete()) {
        return false;
      }
    }
    if (errors == null) {
      return true;
    }

    // Try to make some suggestions
    for (PageElementTag referencesTag : referencesTags) {
      CheckErrorResult errorResult = createCheckErrorResult(
          pageAnalysis.getPage(), referencesTag.getBeginIndex(), referencesTag.getEndIndex());
      if (referencesTags.size() == 1) {
        errorResult.addReplacement("<references />", GT._("Close tag"));
      }
      errors.add(errorResult);
    }

    return true;
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("references_templates", GT._("A list of templates resulting in the inclusion of {0}", "&lt;references/&gt;"));
    return parameters;
  }
}
