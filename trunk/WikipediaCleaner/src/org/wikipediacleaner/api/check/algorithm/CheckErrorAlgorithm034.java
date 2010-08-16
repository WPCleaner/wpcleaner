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

import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;

/**
 * Algorithm for analyzing error 34 of check wikipedia project.
 * Error 34: Template programming element <br/>
 */
public class CheckErrorAlgorithm034 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm034() {
    super("Template programming element");
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#analyze(org.wikipediacleaner.api.data.Page, java.lang.String, java.util.List)
   */
  public boolean analyze(Page page, String contents, List<CheckErrorResult> errors) {
    if ((page == null) || (contents == null)) {
      return false;
    }
    boolean result = false;
    result |= simpleTextSearch(page, contents, errors, "#if:");
    result |= simpleTextSearch(page, contents, errors, "#ifeq:");
    result |= simpleTextSearch(page, contents, errors, "#switch:");
    result |= simpleTextSearch(page, contents, errors, "#tag:");
    result |= simpleTextSearch(page, contents, errors, "{{NAMESPACE}}");
    result |= simpleTextSearch(page, contents, errors, "{{SITENAME}}");
    result |= simpleTextSearch(page, contents, errors, "{{PAGENAME}}", page.getMagicPAGENAME());
    result |= simpleTextSearch(page, contents, errors, "{{FULLPAGENAME}}");
    return result;
  }
}
