/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
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
  @Override
  public Collection<String> getTexts() {
    Collection<String> result = new ArrayList<String>();
    if (image != null) {
      try {
        API api = APIFactory.getAPI();

        // Retrieve image descriptions
        Page imagePage = DataManager.getPage(
            image.getWiki(),
            image.getNamespace() + ":" + image.getImage(),
            null, null, null);
        api.retrieveContents(
            image.getWiki(),
            Collections.singletonList(imagePage), false, false);

        // Use image description on the wiki
        if (Boolean.TRUE.equals(imagePage.isExisting())) {
          PageAnalysis pageAnalysis = imagePage.getAnalysis(imagePage.getContents(), true);
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
            null, null, null);
        api.retrieveContents(
            EnumWikipedia.COMMONS,
            Collections.singletonList(commonsPage), false, false);
        if (Boolean.TRUE.equals(commonsPage.isExisting())) {
          PageAnalysis pageAnalysis = commonsPage.getAnalysis(commonsPage.getContents(), true);
          for (PageElementTemplate template : pageAnalysis.getTemplates()) {
            if (Page.areSameTitle("Information", template.getTemplateName())) {
              String global = template.getParameterValue("Description");
              if ((global != null) && (global.trim().length() > 0)) {
                PageAnalysis descAnalysis = commonsPage.getAnalysis(global, true);
                for (PageElementTemplate template2 : descAnalysis.getTemplates()) {
                  if (Page.areSameTitle(image.getWiki().getSettings().getCode(), template2.getTemplateName())) {
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
