/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.utils;

import java.util.ArrayList;
import java.util.Collection;

import org.wikipediacleaner.api.base.API;
import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.api.base.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * A text provider for image description.
 */
public class TextProviderImageDescription implements TextProvider {

  /**
   * Image.
   */
  private final PageElementImage image;

  /**
   * @param image Image.
   */
  public TextProviderImageDescription(PageElementImage image) {
    this.image = image;
  }

  /**
   * @return Possible texts.
   */
  public Collection<String> getTexts() {
    Collection<String> result = new ArrayList<String>();
    if (image != null) {
      try {
        API api = APIFactory.getAPI();

        // Retrieve image description on the wiki
        Page imagePage = DataManager.getPage(
            image.getWikipedia(),
            image.getNamespace() + ":" + image.getImage(),
            null, null);
        api.retrieveContents(
            image.getWikipedia(),
            imagePage, false);
        if (Boolean.TRUE.equals(imagePage.isExisting())) {
          PageAnalysis pageAnalysis = new PageAnalysis(imagePage, imagePage.getContents());
          for (PageElementTemplate template : pageAnalysis.getTemplates()) {
            if (Page.areSameTitle("Information", template.getTemplateName())) {
              String description = template.getParameterValue("Description");
              if ((description != null) && (description.trim().length() > 0)) {
                result.add(description.trim());
              }
            }
          }
        }

        // Retrieve image description on Commons
        Page commonsPage = DataManager.getPage(
            EnumWikipedia.COMMONS,
            "File:" + image.getImage(),
            null, null);
        api.retrieveContents(
            EnumWikipedia.COMMONS,
            commonsPage, false);
        if (Boolean.TRUE.equals(commonsPage.isExisting())) {
          PageAnalysis pageAnalysis = new PageAnalysis(commonsPage, commonsPage.getContents());
          for (PageElementTemplate template : pageAnalysis.getTemplates()) {
            if (Page.areSameTitle("Information", template.getTemplateName())) {
              String global = template.getParameterValue("Description");
              if ((global != null) && (global.trim().length() > 0)) {
                PageAnalysis descAnalysis = new PageAnalysis(commonsPage, global);
                for (PageElementTemplate template2 : descAnalysis.getTemplates()) {
                  if (Page.areSameTitle(image.getWikipedia().getSettings().getCode(), template2.getTemplateName())) {
                    String description = template2.getParameterValue("1");
                    if ((description != null) && (description.trim().length() > 0)) {
                      result.add(description.trim());
                    }
                  }
                }
              }
            }
          }
        }
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

}
