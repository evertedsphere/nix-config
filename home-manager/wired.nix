# Stolen from oddllama
# https://github.com/oddlama/nix-config/blob/2fece6e5cc7cd49ba2104ba1725bafb703ea85b4/pkgs/default.nix
{
  config,
  lib,
  pkgs,
  ...
}: {
  systemd.user.services.wired.Unit.ConditionEnvironment = "DISPLAY";
  services.wired = {
    enable = true;
    config = let
      inherit (builtins) floor;

      inner = config.xsession.windowManager.i3.config.gaps.inner;
      border = config.xsession.windowManager.i3.config.window.border;

      format = pkgs.formats.ron {};
      inherit (format.lib) mkLiteral struct;

      style = config.lib.stylix.colors;

      inherit (config.stylix) fonts;

      # A global scaling factor to apply to all notifications.
      globalScale = 1.6;

      # Ron format shorthands and helpers
      unnamedStruct = struct "";
      Color = hex: struct "Color" {inherit hex;};
      Hook = struct "Hook";
      Vec2 = x: y: struct "Vec2" {inherit x y;};
      ShortcutsConfig = struct "ShortcutsConfig";
      And = struct "And";
      Or = struct "Or";
      Not = struct "Not";

      mkVec2 = x: y: Vec2 (globalScale * x) (globalScale * y);

      mkHook = parent_anchor: self_anchor:
        Hook {
          parent_anchor = mkLiteral parent_anchor;
          self_anchor = mkLiteral self_anchor;
        };

      mkPaddingLrBt = left: right: bottom: top:
        struct "Padding" {
          left = globalScale * left;
          right = globalScale * right;
          bottom = globalScale * bottom;
          top = globalScale * top;
        };

      mkDimensionsWH = minw: maxw: minh: maxh:
        unnamedStruct {
          width = unnamedStruct {
            min = floor (globalScale * minw);
            max = floor (globalScale * maxw);
          };
          height = unnamedStruct {
            min = floor (globalScale * minh);
            max = floor (globalScale * maxh);
          };
        };

      colors = lib.mapAttrs (_: Color) style.withHashtag;
      bg-colors = {
        base00 = Color "#e0${style.base00}";
        base01 = Color "#e0${style.base01}";
        base02 = Color "#e0${style.base02}";
        base04 = Color "#e0${style.base04}";
        base05 = Color "#e0${style.base05}";
        base06 = Color "#e0${style.base06}";
        base08 = Color "#e0${style.base08}";
        base09 = Color "#e0${style.base09}";
      };

      notificationBlock = unnamedStruct {
        monitor = 0;
        border_width = globalScale * 2;
        border_rounding = globalScale * 0;
        background_color = bg-colors.base00;
        border_color = bg-colors.base00;
        border_color_low = bg-colors.base00;
        border_color_critical = bg-colors.base08;
        border_color_paused = bg-colors.base09;
        gap = mkVec2 0 inner;
        notification_hook = mkHook "BR" "TR";
      };

      ma = 5;
      mb = 10;

      mkTopBar = name: extra:
        map (x: extra // x) [
          {
            name = "${name}_app_image";
            parent = "${name}_root";
            hook = mkHook "TL" "TL";
            offset = mkVec2 0 0;
            params = struct "ImageBlock" (unnamedStruct {
              filter_mode = mkLiteral "Lanczos3";
              image_type = mkLiteral "App";
              min_height = floor (globalScale * 24);
              min_width = floor (globalScale * 24);
              padding = mkPaddingLrBt mb ma ma mb;
              rounding = globalScale * 12.0;
              scale_height = floor (globalScale * 24);
              scale_width = floor (globalScale * 24);
            });
          }
          {
            name = "${name}_app_name";
            parent = "${name}_app_image";
            hook = mkHook "TR" "TL";
            offset = mkVec2 0 0;
            params = struct "TextBlock" (unnamedStruct {
              color = colors.base04;
              dimensions = mkDimensionsWH 350 350 28 28;
              ellipsize = mkLiteral "End";
              font = "${fonts.sansSerif.name} ${toString (globalScale * 14)}";
              padding = mkPaddingLrBt ma 0 0 mb;
              text = "%n";
            });
          }
        ];

      mkBody = name: ident: yOffset: summaryRightPadding: extra: let
        maxWFull = 580 - 12;
        maxWImg = maxWFull - 128 - 12;
      in
        map (x: extra // x) (
          [
            {
              name = "${name}_${ident}_hint";
              parent = "${name}_root";
              hook = mkHook "TL" "TL";
              offset = mkVec2 0 yOffset;
              params = struct "ImageBlock" (unnamedStruct {
                filter_mode = mkLiteral "Lanczos3";
                image_type = mkLiteral "Hint";
                padding = mkPaddingLrBt mb ma mb mb;
                rounding = globalScale * 9;
                scale_height = floor (globalScale * 128);
                scale_width = floor (globalScale * 128);
              });
            }
            {
              name = "${name}_${ident}_summary";
              parent = "${name}_${ident}_hint";
              hook = mkHook "TR" "TL";
              offset = mkVec2 0 0;
              params = struct "TextBlock" (unnamedStruct {
                text = "%s";
                font = "${fonts.sansSerif.name} Bold ${toString (globalScale * 18)}";
                ellipsize = mkLiteral "End";
                color = colors.base05;
                padding = mkPaddingLrBt mb 0 0 ma;
                dimensions = mkDimensionsWH (maxWFull - summaryRightPadding) (maxWFull - summaryRightPadding) 0 30;
                dimensions_image_hint = mkDimensionsWH (maxWImg - summaryRightPadding) (maxWImg - summaryRightPadding) 0 30;
                dimensions_image_both = mkDimensionsWH (maxWImg - summaryRightPadding) (maxWImg - summaryRightPadding) 0 30;
              });
            }
            {
              name = "${name}_${ident}_body";
              parent = "${name}_${ident}_summary";
              hook = mkHook "BL" "TL";
              offset = mkVec2 0 0;
              render_criteria = [
                (And [
                  (Or extra.render_criteria)
                  (mkLiteral "Body")
                ])
              ];
              params = struct "TextBlock" (unnamedStruct {
                text = "%b";
                font = "${fonts.sansSerif.name} ${toString (globalScale * 16)}";
                ellipsize = mkLiteral "End";
                color = colors.base05;
                padding = mkPaddingLrBt mb 0 mb ma;
                dimensions = mkDimensionsWH maxWFull maxWFull 0 88;
                dimensions_image_hint = mkDimensionsWH maxWImg maxWImg 0 88;
                dimensions_image_both = mkDimensionsWH maxWImg maxWImg 0 88;
              });
            }
          ]
          # We unfortunately cannot move these out of mkBody, because the depend
          # on the specific name for the parent, which cannot be changed dynamically.
          # So each call to mkBody creates these progress bars which only differ in
          # their parent :/
          ++ (mkProgress name "${ident}_hint" (-4) {
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (mkLiteral "Progress")
                  (mkLiteral "HintImage")
                ]))
            ];
          })
          ++ (mkProgress name "${ident}_body" 0 {
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (mkLiteral "Progress")
                  (mkLiteral "Body")
                  (Not (mkLiteral "HintImage"))
                ]))
            ];
          })
          ++ (mkProgress name "${ident}_summary" 0 {
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (mkLiteral "Progress")
                  (mkLiteral "Summary")
                  (Not (Or [
                    (mkLiteral "Body")
                    (mkLiteral "HintImage")
                  ]))
                ]))
            ];
          })
          # Each mkProgress includes a button bar. But if no progress is included in a notification,
          # those won't be rendered, so we have to define bars for non-progress notifications.
          # (And yes, we need 3 because we cannot have duplicate names or dynamic parents)
          ++ (mkButtonBar name "${ident}_hint" {
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (Not (mkLiteral "Progress"))
                  (mkLiteral "HintImage")
                ]))
            ];
          })
          ++ (mkButtonBar name "${ident}_body" {
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (Not (mkLiteral "Progress"))
                  (mkLiteral "Body")
                  (Not (mkLiteral "HintImage"))
                ]))
            ];
          })
          ++ (mkButtonBar name "${ident}_summary" {
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (Not (mkLiteral "Progress"))
                  (mkLiteral "Summary")
                  (Not (Or [
                    (mkLiteral "Body")
                    (mkLiteral "HintImage")
                  ]))
                ]))
            ];
          })
        );

      mkProgress = name: parent: yOffset: extra:
        map (x: extra // x) (
          [
            {
              name = "${name}_progress_${parent}_text";
              parent = "${name}_${parent}";
              hook = mkHook "BL" "TL";
              offset = mkVec2 0 0;
              params = struct "TextBlock" (unnamedStruct {
                text = "%p%";
                font = "${fonts.monospace.name} Bold ${toString (globalScale * 14)}";
                align = mkLiteral "Right";
                ellipsize = mkLiteral "End";
                color = bg-colors.base06;
                padding = mkPaddingLrBt 12 16 12 yOffset;
                dimensions = mkDimensionsWH 48 48 24 24;
              });
            }
            {
              name = "${name}_progress_${parent}_bar";
              parent = "${name}_${parent}";
              hook = mkHook "BL" "TL";
              offset = mkVec2 0 0;
              params = struct "ProgressBlock" (unnamedStruct {
                width = globalScale * 524;
                height = globalScale * 12;
                border_width = 0.0;
                border_rounding = globalScale * 4;
                border_color = colors.base03;
                background_color = colors.base03;
                fill_color = colors.base0D;
                fill_rounding = globalScale * 4;
                padding = mkPaddingLrBt 68 16 12 (yOffset + 8);
              });
            }
          ]
          ++ (mkButtonBar name "progress_${parent}_text" extra)
        );

      mkButtonBar = name: parent: extra:
        map (x: extra // x) (lib.flatten [
          {
            name = "${name}_button_bar_for_${parent}";
            parent = "${name}_${parent}";
            hook = mkHook "BL" "TL";
            offset = mkVec2 0 0;
            render_criteria = [
              (And (extra.render_criteria
                ++ [
                  (Or [
                    (struct "ActionOther" 0)
                    (struct "ActionOther" 1)
                    (struct "ActionOther" 2)
                    (struct "ActionOther" 3)
                    (struct "ActionOther" 4)
                    (struct "ActionOther" 5)
                  ])
                ]))
            ];
            params = struct "TextBlock" (unnamedStruct {
              text = "";
              font = "${fonts.sansSerif.name} 1";
              color = colors.base06;
              padding = mkPaddingLrBt 0 0 0 0;
              dimensions = mkDimensionsWH 568 568 44 44;
            });
          }
          (lib.flip map [0 1 2 3 4 5] (
            i:
              lib.optionalAttrs (i == 0) {
                parent = "${name}_button_bar_for_${parent}";
                hook = mkHook "TL" "TL";
                offset = mkVec2 16 0;
              }
              // lib.optionalAttrs (i != 0) {
                parent = "${name}_action_${toString (i - 1)}_for_${parent}";
                hook = mkHook "TR" "TL";
                offset = mkVec2 8 0;
              }
              // {
                name = "${name}_action_${toString i}_for_${parent}";
                render_criteria = [
                  (
                    And (extra.render_criteria
                      ++ [
                        (struct "ActionOther" i)
                      ])
                  )
                ];
                params = struct "ButtonBlock" (unnamedStruct {
                  text = "%a";
                  font = "${fonts.monospace.name} Bold ${toString (globalScale * 14)}";
                  ellipsize = mkLiteral "End";
                  action = struct "OtherAction" i;
                  text_color = colors.base06;
                  text_color_hovered = colors.base06;
                  background_color = colors.base01;
                  border_color = colors.base02;
                  background_color_hovered = colors.base02;
                  border_color_hovered = colors.base04;
                  border_rounding = globalScale * 6;
                  border_width = globalScale * 1;
                  padding = mkPaddingLrBt 8 8 4 4;
                  # Technically distribute like below, but we'll just allow more even
                  # if it breaks when having > 4 max length buttons, because it probably
                  # never happens and looks a lot better this way.
                  dimensions = mkDimensionsWH 32 144 24 24;
                  # dimensions = mkDimensionsWH 32 ((
                  #   568 /* available width */
                  #   - 2 * 16 /* padding lr */
                  #   - (/* count actions */ 6 - 1) * 8 /* padding between */
                  # ) / /* count actions */ 6) 24 24;
                });
              }
          ))
        ]);

      mkIndicatorValue = name: ident: parent: extra: textParamsExtra: progressParamsExtra:
        map (x: extra // x) (lib.flatten [
          {
            name = "${name}_${ident}_value_text";
            parent = "${name}_${parent}";
            hook = mkHook "BM" "TM";
            offset = mkVec2 0 0;
            params = struct "TextBlock" (unnamedStruct ({
                text = "%p%";
                font = "${fonts.monospace.name} Bold ${toString (globalScale * 18)}";
                align = mkLiteral "Center";
                color = colors.base06;
                padding = mkPaddingLrBt 0 0 999 0;
                dimensions = mkDimensionsWH 64 64 32 32;
              }
              // textParamsExtra));
          }
          {
            name = "${name}_${ident}_value_bar";
            parent = "${name}_${ident}_value_text";
            hook = mkHook "BM" "TM";
            offset = mkVec2 0 0;
            params = struct "ProgressBlock" (unnamedStruct ({
                width = globalScale * 0.90 * 384;
                height = globalScale * 16;
                border_width = 0.0;
                border_rounding = globalScale * 6;
                border_color = colors.base03;
                background_color = colors.base03;
                fill_color = colors.base0F;
                fill_rounding = globalScale * 6;
                padding = mkPaddingLrBt 0 0 0 0;
              }
              // progressParamsExtra));
          }
        ]);
    in
      format.generate "wired.ron" (unnamedStruct {
        max_notifications = 10;
        timeout = 60000;
        poll_interval = 6; # 6ms ~= 166hz.
        history_length = 60;
        replacing_enabled = true;
        replacing_resets_timeout = true;
        min_window_width = floor (globalScale * 20);
        min_window_height = floor (globalScale * 20);
        debug = false;
        debug_color = Color "#33ff0000";
        debug_color_alt = Color "#330000ff";

        # https://github.com/Toqozz/wired-notify/wiki/Shortcuts
        shortcuts = ShortcutsConfig {
          notification_interact = 1; # left click
          notification_pause = 1;
          notification_close = 2; # right click
          notification_action1 = 3; # middle click
        };

        layout_blocks = let
          criterionHasTopBar = And [
            (mkLiteral "AppImage")
            (Not (Or [
              (struct "AppName" "")
              (struct "AppName" "notify-send")
              (struct "AppName" "Spotify")
            ]))
          ];
        in
          map unnamedStruct (lib.flatten [
            # Root block for normal notifications
            {
              name = "general_root";
              parent = "";
              hook = mkHook "TR" "TR";
              offset = let
                bar-height = 48;
                margin = inner * 2;
                right = inner + border + margin;
                top = bar-height + inner + border + margin;
              in
                Vec2 (-right) top; # Vec2 instead of mkVec2 to not apply scaling.
              render_criteria = [
                (Not (Or [
                  (struct "Tag" "indicator")
                ]))
              ];
              params = struct "NotificationBlock" notificationBlock;
            }
            # Dummy text that enforces minimum window size
            {
              name = "general_size";
              parent = "general_root";
              hook = mkHook "TL" "TL";
              offset = Vec2 0 0;
              params = struct "TextBlock" (unnamedStruct {
                text = "";
                font = "${fonts.sansSerif.name} 1";
                color = colors.base09;
                padding = mkPaddingLrBt 0 0 0 0;
                dimensions = mkDimensionsWH 600 600 1 1;
              });
            }
            # Time is always shown in the top right corner.
            {
              name = "general_time";
              parent = "general_root";
              hook = mkHook "TL" "TL";
              offset = mkVec2 600 0;
              params = struct "TextBlock" (unnamedStruct {
                color = colors.base03;
                dimensions = mkDimensionsWH 80 80 28 28;
                ellipsize = mkLiteral "End";
                font = "${fonts.sansSerif.name} ${toString (globalScale * 14)}";
                padding = mkPaddingLrBt 0 mb 0 mb;
                text = "%t(%H:%M:%S)";
              });
            }
            # Top bar for app image, name and time, but only
            # if there is an app name or image.
            (mkTopBar "general" {
              render_criteria = [criterionHasTopBar];
            })
            # if no top bar present: A body with no offset and a summary padding to the right (to not overlay the time)
            (mkBody "general" "notop" 0 (16 + 100) {
              render_criteria = [(Not criterionHasTopBar)];
            })
            # if top bar present: A body with matching y offset and no summary padding to the right
            (mkBody "general" "withtop" 36 0 {
              render_criteria = [criterionHasTopBar];
            })

            # # Root block for brightness/volume indicators
            # {
            #   name = "indicator_root";
            #   parent = "";
            #   hook = mkHook "MM" "MM";
            #   offset = Vec2 0 0; # Vec2 instead of mkVec2 to not apply scaling.
            #   render_criteria = [
            #     (And [
            #       (struct "Tag" "indicator")
            #     ])
            #   ];
            #   params = struct "NotificationBlock" (unnamedStruct {
            #     monitor = 0;
            #     border_width = globalScale * 2;
            #     border_rounding = globalScale * 0;
            #     background_color = colors.base00;
            #     border_color = colors.base04;
            #     border_color_low = colors.base04;
            #     border_color_critical = colors.base08;
            #     border_color_paused = colors.base09;
            #     gap = mkVec2 0 0;
            #     notification_hook = mkHook "MM" "MM";
            #   });
            # }
            # # Dummy text that enforces minimum window size
            # {
            #   name = "indicator_size";
            #   parent = "indicator_root";
            #   hook = mkHook "TL" "TL";
            #   offset = Vec2 0 0;
            #   params = struct "TextBlock" (unnamedStruct {
            #     text = "";
            #     font = "${fonts.monospace.name} 1";
            #     color = colors.base06;
            #     padding = mkPaddingLrBt 0 0 0 0;
            #     dimensions = mkDimensionsWH 384 384 384 384;
            #   });
            # }
            # {
            #   name = "indicator_summary";
            #   parent = "indicator_size";
            #   hook = mkHook "TM" "TM";
            #   offset = mkVec2 0 0;
            #   params = struct "TextBlock" (unnamedStruct {
            #     text = "%s";
            #     font = "${fonts.sansSerif.name} Bold ${toString (globalScale * 18)}";
            #     align = mkLiteral "Center";
            #     color = colors.base06;
            #     padding = mkPaddingLrBt 0 0 8 8;
            #     dimensions = mkDimensionsWH 300 300 32 32;
            #   });
            # }
            # {
            #   name = "indicator_hint";
            #   parent = "indicator_summary";
            #   hook = mkHook "BM" "TM";
            #   offset = mkVec2 0 0;
            #   params = struct "ImageBlock" (unnamedStruct {
            #     filter_mode = mkLiteral "Lanczos3";
            #     image_type = mkLiteral "Hint";
            #     min_height = floor (globalScale * 180);
            #     min_width = floor (globalScale * 180);
            #     padding = mkPaddingLrBt 0 0 (35 + 8) 35;
            #     rounding = globalScale * 9;
            #     scale_height = floor (globalScale * 180);
            #     scale_width = floor (globalScale * 180);
            #   });
            # }
            # (
            #   mkIndicatorValue "indicator" "anything" "hint" {
            #     render_criteria = [
            #       (Not (Or [
            #         (struct "Note" "brightness")
            #         (struct "Note" "volume")
            #       ]))
            #     ];
            #   }
            #   # text extra
            #   {}
            #   # progress extra
            #   {}
            # )
            # (mkIndicatorValue "indicator" "brightness" "hint" {
            #     render_criteria = [
            #       (And [
            #         (struct "Note" "brightness")
            #       ])
            #     ];
            #   }
            #   # text extra
            #   {}
            #   # progress extra
            #   {
            #     fill_color = colors.base0A;
            #   })
            # (mkIndicatorValue "indicator" "volume" "hint" {
            #     render_criteria = [
            #       (And [
            #         (struct "Note" "volume")
            #       ])
            #     ];
            #   }
            #   # text extra
            #   {}
            #   # progress extra
            #   {
            #     fill_color = colors.base0B;
            #   })
          ]);
      });
  };
}
