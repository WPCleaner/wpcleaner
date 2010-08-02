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

import java.util.ArrayList;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TagBlock;
import org.wikipediacleaner.api.data.TemplateBlock;


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
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(Page page, String contents, ArrayList<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }

    // Analyzing the text for <ref>
    boolean refFound = false;
    if (!refFound) {
      // Search for <ref>
      TagBlock tag = findNextTag(page, contents, "ref", 0);
      if (tag != null) {
        refFound = true;
      }
    }

    // Analyzing the text for <references>
    boolean referencesFound = false;
    if (refFound) {
      // Search for <references>
      if (!referencesFound) {
        TagBlock tag = findNextTag(page, contents, "references", 0);
        if (tag != null) {
          referencesFound = true;
        }
      }
      // Search for {{Références}}
      String[] referencesTemplates = {
        "Références", "References", "Notes", "Reflist"  
      }; // TODO: Use error_003_references_templates
      for (String referencesTemplate : referencesTemplates) {
        if (!referencesFound) {
          TemplateBlock template = findNextTemplate(page, contents, referencesTemplate, 0);
          if (template != null) {
            referencesFound = true;
          }
        }
      }
    }

    // Result
    if (refFound && !referencesFound) {
      return true;
    }
    return false;
  }
}
