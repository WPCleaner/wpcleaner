/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 11 of check wikipedia project.
 * Error 11: HTML named entities
 */
public class CheckErrorAlgorithm011 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm011() {
    super("HTML named entities");
    htmlCharacters = new ArrayList<HtmlCharacters>();
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_A_ACUTE_ACCENT);         // &aacute;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_A_ACUTE_ACCENT);       // &Aacute;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_A_CIRCUMFLEX_ACCENT);    // &acirc;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_A_CIRCUMFLEX_ACCENT);  // &Acirc;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_AE);                     // &aelig;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_AE);                   // &AElig;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_A_GRAVE_ACCENT);         // &agrave;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_A_GRAVE_ACCENT);       // &Agrave;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_ALPHA);                  // &alpha;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_ALPHA);                // &Alpha;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_A_RING);                 // &aring;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_A_RING);               // &Aring;
    htmlCharacters.add(HtmlCharacters.SYMBOL_ALMOST_EQUAL);                 // &asymp;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_A_TILDE);                // &atilde;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_A_TILDE);              // &Atilde;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_A_UMLAUT_MARK);          // &auml;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_A_UMLAUT_MARK);        // &Auml;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DOUBLE_LOW_9_QUOTATION_MARK);  // &bdquo;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_BETA);                   // &beta;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_BETA);                 // &Beta;
    htmlCharacters.add(HtmlCharacters.SYMBOL_BROKEN_VERTICAL_BAR);          // &brvbar;
    htmlCharacters.add(HtmlCharacters.SYMBOL_BULLET);                       // &bull;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_C_CEDILLA);              // &ccedil;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_C_CEDILLA);            // &Ccedil;
    htmlCharacters.add(HtmlCharacters.SYMBOL_CENT);                         // &cent;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_CHI);                    // &chi;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_CHI);                  // &Chi;
    htmlCharacters.add(HtmlCharacters.SYMBOL_CLUB);                         // &clubs;
    htmlCharacters.add(HtmlCharacters.SYMBOL_COPYRIGHT);                    // &copy;
    htmlCharacters.add(HtmlCharacters.SYMBOL_CARRIAGE_RETURN_ARROW);        // &crarr;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_DAGGER);                       // &dagger;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DOUBLE_DAGGER);                // &Dagger;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DOWN_ARROW);                   // &darr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DOWN_DOUBLE_ARROW);            // &dArr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DEGREE);                       // &deg;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_DELTA);                  // &delta;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_DELTA);                // &Delta;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DIAMOND);                      // &diams;
    htmlCharacters.add(HtmlCharacters.SYMBOL_DIVISION);                     // &divide;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_E_ACUTE_ACCENT);         // &eacute;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_E_ACUTE_ACCENT);       // &Eacute;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_E_CIRCUMFLEX_ACCENT);    // &ecirc;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_E_CIRCUMFLEX_ACCENT);  // &Ecirc;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_E_GRAVE_ACCENT);         // &egrave;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_E_GRAVE_ACCENT);       // &Egrave;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_EPSILON);                // &epsilon;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_EPSILON);              // &Epsilon;
    htmlCharacters.add(HtmlCharacters.SYMBOL_EQUIVALENT);                   // &equiv;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_ETA);                    // &eta;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_ETA);                  // &Eta;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_ETH);                    // &eth;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_ETH);                  // &ETH;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_E_UMLAUT_MARK);          // &euml;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_E_UMLAUT_MARK);        // &Euml;
    htmlCharacters.add(HtmlCharacters.SYMBOL_EURO);                         // &euro;
    htmlCharacters.add(HtmlCharacters.LETTER_F_WITH_HOOK);                  // &fnof;
    htmlCharacters.add(HtmlCharacters.SYMBOL_FRACTION_1_2);                 // &frac12;
    htmlCharacters.add(HtmlCharacters.SYMBOL_FRACTION_1_4);                 // &frac14;
    htmlCharacters.add(HtmlCharacters.SYMBOL_FRACTION_3_4);                 // &frac34;
    htmlCharacters.add(HtmlCharacters.SYMBOL_FRACTION_SLASH);               // &frasl;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_GAMMA);                  // &gamma;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_GAMMA);                // &Gamma;
    htmlCharacters.add(HtmlCharacters.SYMBOL_GREATER_OR_EQUAL);             // &ge;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_RIGHT_ARROW);             // &harr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_RIGHT_DOUBLE_ARROW);      // &hArr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_HEART);                        // &hearts;
    htmlCharacters.add(HtmlCharacters.SYMBOL_HORIZONTAL_ELLIPSIS);          // &hellip;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_I_ACUTE_ACCENT);         // &iacute;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_I_ACUTE_ACCENT);       // &Iacute;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_I_CIRCUMFLEX_ACCENT);    // &icirc;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_I_CIRCUMFLEX_ACCENT);  // &Icirc;
    htmlCharacters.add(HtmlCharacters.SYMBOL_INVERTED_EXCLAMATION_MARK);    // &iexcl;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_I_GRAVE_ACCENT);         // &igrave;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_I_GRAVE_ACCENT);       // &Igrave;
    htmlCharacters.add(HtmlCharacters.SYMBOL_INFINITY);                     // &infin;
    htmlCharacters.add(HtmlCharacters.SYMBOL_INTEGRAL);                     // &int;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_IOTA);                   // &iota;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_IOTA);                 // &Iota;
    htmlCharacters.add(HtmlCharacters.SYMBOL_INVERTED_QUESTION_MARK);       // &iquest;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_I_UMLAUT_MARK);          // &iuml;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_I_UMLAUT_MARK);        // &Iuml;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_KAPPA);                  // &kappa;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_KAPPA);                // &Kappa;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_LAMBDA);                 // &lambda;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_LAMBDA);               // &Lambda;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_ANGLE_QUOTATION_MARK);    // &laquo;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_ARROW);                   // &larr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_DOUBLE_ARROW);            // &lArr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_DOUBLE_QUOTATION_MARK);   // &ldquo;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LESS_OR_EQUAL);                // &le;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LOZENGE);                      // &loz;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SINGLE_LEFT_ANGLE_QUOTATION);  // &lsaquo;
    htmlCharacters.add(HtmlCharacters.SYMBOL_LEFT_SINGLE_QUOTATION_MARK);   // &lsquo;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_EM_DASH);                      // &mdash;
    htmlCharacters.add(HtmlCharacters.SYMBOL_MICRO);                        // &micro;
    htmlCharacters.add(HtmlCharacters.SYMBOL_MIDDLE_DOT);                   // &middot;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_MINUS);                        // &minus;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_MU);                     // &mu;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_MU);                   // &Mu;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_EN_DASH);                      // &ndash;
    htmlCharacters.add(HtmlCharacters.SYMBOL_NOT_EQUAL);                    // &ne;
    htmlCharacters.add(HtmlCharacters.SYMBOL_NEGATION);                     // &not;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_N_TILDE);                // &ntilde;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_N_TILDE);              // &Ntilde;
    // htmlCharacters.add(HtmlCharacters.LETTER_SMALL_NU);                     // &nu;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_NU);                   // &Nu;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_O_ACUTE_ACCENT);         // &oacute;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_O_ACUTE_ACCENT);       // &Oacute;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_O_CIRCUMFLEX_ACCENT);    // &ocirc;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_O_CIRCUMFLEX_ACCENT);  // &Ocirc;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_OE);                     // &oelig;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_OE);                   // &OElig;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_O_GRAVE_ACCENT);         // &ograve;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_O_GRAVE_ACCENT);       // &Ograve;
    htmlCharacters.add(HtmlCharacters.SYMBOL_OVERLINE);                     // &oline;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_OMEGA);                  // &omega;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_OMEGA);                // &Omega;
    // htmlCharacters.add(HtmlCharacters.LETTER_SMALL_OMICRON);                // &omicron;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_OMICRON);              // &Omicron;    
    htmlCharacters.add(HtmlCharacters.SYMBOL_FEMININE_ORDINAL_INDICATOR);   // &ordf;
    htmlCharacters.add(HtmlCharacters.SYMBOL_MASCULINE_ORDINAL_INDICATOR);  // &ordm;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_O_SLASH);                // &oslash;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_O_SLASH);              // &Oslash;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_O_TILDE);                // &otilde;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_O_TILDE);              // &Otilde;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_O_UMLAUT_MARK);          // &ouml;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_O_UMLAUT_MARK);        // &Ouml;
    htmlCharacters.add(HtmlCharacters.SYMBOL_PARAGRAPH);                    // &para;
    htmlCharacters.add(HtmlCharacters.SYMBOL_PART);                         // &part;
    htmlCharacters.add(HtmlCharacters.SYMBOL_PER_MILLE);                    // &permil;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_PHI);                    // &phi;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_PHI);                  // &Phi;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_PI);                     // &pi;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_PI);                   // &Pi;
    htmlCharacters.add(HtmlCharacters.LETTER_PI_SYMBOL);                    // &piv;
    htmlCharacters.add(HtmlCharacters.SYMBOL_PLUS_OR_MINUS);                // &plusm;
    htmlCharacters.add(HtmlCharacters.SYMBOL_POUND);                        // &pound;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_MINUTES);                      // &prime;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_SECONDS);                      // &Prime;
    htmlCharacters.add(HtmlCharacters.SYMBOL_PROD);                         // &prod;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_PSI);                    // &psi;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_PSI);                  // &Psi;
    htmlCharacters.add(HtmlCharacters.SYMBOL_QUOTATION_MARK);               // &quot;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SQUARE_ROOT);                  // &radic;
    htmlCharacters.add(HtmlCharacters.SYMBOL_RIGHT_ANGLE_QUOTATION_MARK);   // &raquo;
    htmlCharacters.add(HtmlCharacters.SYMBOL_RIGHT_ARROW);                  // &rarr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_RIGHT_DOUBLE_ARROW);           // &rArr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_RIGHT_DOUBLE_QUOTATION_MARK);  // &rdquo;
    htmlCharacters.add(HtmlCharacters.SYMBOL_REGISTERED_TRADEMARK);         // &reg;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_RHO);                    // &rho;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_RHO);                  // &Rho;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SINGLE_RIGHT_ANGLE_QUOTATION); // &rsaquo;
    htmlCharacters.add(HtmlCharacters.SYMBOL_RIGHT_SINGLE_QUOTATION_MARK);  // &rsquo;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_S_CARON);                // &scaron;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_S_CARON);              // &Scaron;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SECTION);                      // &sect;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_SIGMA);                  // &sigma;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_SIGMA);                // &Sigma;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_SIGMAF);                 // &sigmaf;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SPADE);                        // &spades;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SUM);                          // &sum;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SUPERSCRIPT_1);                // &sup1;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SUPERSCRIPT_2);                // &sup2;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SUPERSCRIPT_3);                // &sup3;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_SHARP_S);                // &szlig;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_TAU);                    // &tau;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_TAU);                  // &Tau;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_THETA);                  // &theta;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_THETA);                // &Theta;
    htmlCharacters.add(HtmlCharacters.LETTER_THETA_SYMBOL);                 // &thetasym;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_THORN);                  // &thorn;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_THORN);                // &THORN;
    htmlCharacters.add(HtmlCharacters.SYMBOL_SMALL_TILDE);                  // &tilde;
    // htmlCharacters.add(HtmlCharacters.SYMBOL_MULTIPLICATION);               // &times;
    htmlCharacters.add(HtmlCharacters.SYMBOL_TRADEMARK);                    // &trade;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_U_ACUTE_ACCENT);         // &uacute;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_U_ACUTE_ACCENT);       // &Uacute;
    htmlCharacters.add(HtmlCharacters.SYMBOL_UP_ARROW);                     // &uarr;
    htmlCharacters.add(HtmlCharacters.SYMBOL_UP_DOUBLE_ARROW);              // &uArr;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_U_CIRCUMFLEX_ACCENT);    // &ucirc;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_U_CIRCUMFLEX_ACCENT);  // &Ucirc;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_U_GRAVE_ACCENT);         // &ugrave;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_U_GRAVE_ACCENT);       // &Ugrave;
    htmlCharacters.add(HtmlCharacters.LETTER_UPSILON_SYMBOL);               // &upsih;
    // htmlCharacters.add(HtmlCharacters.LETTER_SMALL_UPSILON);                // &upsilon;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_UPSILON);              // &Upsilon;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_U_UMLAUT_MARK);          // &uuml;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_U_UMLAUT_MARK);        // &Uuml;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_XI);                     // &xi;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_XI);                   // &Xi;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_Y_ACUTE_ACCENT);         // &yacute;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_Y_ACUTE_ACCENT);       // &Yacute;
    htmlCharacters.add(HtmlCharacters.SYMBOL_YEN);                          // &yen;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_Y_UMLAUT_MARK);          // &yuml;
    htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_Y_UMLAUT_MARK);        // &Yuml;
    htmlCharacters.add(HtmlCharacters.LETTER_SMALL_ZETA);                   // &zeta;
    // htmlCharacters.add(HtmlCharacters.LETTER_CAPITAL_ZETA);                 // &Zeta;
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // If math tags are present, don't report the error
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_WIKI_MATH);
    if ((tags != null) && (!tags.isEmpty())) {
      for (PageElementTag tag : tags) {
        int index = tag.getBeginIndex();
        if ((analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) == null) &&
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) == null) &&
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) == null)) {
          return false;
        }
      }
    }

    // If math templates are present, don't report the error
    List<PageElementTemplate> templates = analysis.getTemplates("math");
    if ((templates != null) && (!templates.isEmpty())) {
      return false;
    }

    return super.analyze(analysis, errors, onlyAutomatic);
  }
}
