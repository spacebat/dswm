#!/usr/bin/env perl

#
# See also: http://sph-info.eu/textual/computer/programs/org2dokuwiki
#
# Usage
#
# cat <<EOF | ./org2doku
# * heading
#
# - item
# - sub item
#
# ** sub heading
# EOF
# ===== heading =====
#
# * item
# * sub item
#
# ==== sub heading ====
#
# Todo
#
# * font style
#
# Copyright
#
# Copyright (c) 2011 Takumi KINJO
#
# License
#
# The MIT License. See http://www.opensource.org/licenses/mit-license.php

$item_level = 0;

@last_item_indents = ();

$is_published = 1;
$last_head_indent = 0;
$is_code_block = 0;

@code_block_buf = ();

sub next_item_level {
    ($len, $level) = @_;
    $index = $#last_item_indents;
    if ($index >= 0) {
        if ($len == $last_item_indents[$index]) {
            return $level;
        } else {
            for ($i = $index; $i >= 0; $i--) {
                $last_item_indent = $last_item_indents[$i];
                if ($len > $last_item_indent) {
                    push @last_item_indents, $len;
                    return $level + 1;
                } elsif ($len == $last_item_indent) {
                    return $level - ($index - $i);
                }
                pop @last_item_indents;
            }
            return $level;
        }
    } else {
        push @last_item_indents, $len;
        return $level + 1;
    }
}

sub doku_link {
    ($org_hlink) = @_;
    while ($org_hlink =~ s/\[\[(.*?)\]\[(.*?)\]\]/[[$1|$2]]/g) {}
    return $org_hlink;
}

sub shallowest_indent {
    (@last_item_indents) = @_;
    $shallowest_indent = 128;
    foreach $code_line (@last_item_indents) {
        if ($code_line =~ /^(\s+).*/) {
            $len = length($1);
            if ($len < $shallowest_indent) {
                $shallowest_indent = $len;
            }
        } else {
            $shallowest_indent = 0;
        }
    }
    return $shallowest_indent;
}

while (<>) {
    chomp;
    $line = $_;

 retry:
    if ($is_published) {
        if (/^=\ (.*)/) {
            # howm header
            $line = $1;
            $line =~ s/<<<//g;
            $line =~ s/^/====== /g;
            $line =~ s/$/ ======/g;
            $item_level = 0;
            @last_item_indents = ();

        } elsif (/^(\*+)\ (.*)/) {
            # org heading
            $last_head_indent = length($1);
            if ($2 =~ /^!/) {
                $is_published = 0;
                next;
            }
            $doku_heading = ("=" x (6 - length($1)));
            $line = $doku_heading." $2 ".$doku_heading;
            $item_level = 0;
            @last_item_indents = ();

        } elsif (/^(\s+)[-+*]\ (.*)/) {
            # org item
            $item_level = next_item_level(length($1), $item_level);
            $line = (" " x $item_level)."* ".$2;

        } elsif (/^#\+begin_src\s*\w*|#\+begin_example\s*\w*/i) {
            # org code block begin
            $item_level = 0;
            @last_item_indents = ();
            $is_code_block = 1;
            next;

        } elsif (/^#\+end_src|^#\+end_example/i) {
            # org code block end
            $item_level = 0;
            @last_item_indents = ();
            $is_code_block = 0;
            print "<code>\n";
            $shallowest_indent = shallowest_indent(@code_block_buf);
            foreach $line (@code_block_buf) {
                $regex = "\ " x $shallowest_indent;
                $line =~ s/^$regex//g;
                $line =~ s/^,\*/*/g;
                print $line."\n";
            }
            print "</code>\n";
            @code_block_buf = ();
            next;

        } else {
            # paragraph
            if (!$is_code_block) {
                if ($line =~ /(.*?)\ \*([^\s]+)\*\ (.*)/) {
                    # bold
                    $line = $1." **".$2."** ".$3;
                }
                if ($line =~ /(.*?)\ _([^\s]+)_\ (.*)/) {
                    # under line
                    $line = $1." __".$2."__ ".$3;
                }
            }
            $line =~ s/^\s+//g;
            if ($line) {
                $item_level = 0;
                @last_item_indents = ();
            }
        }
        if ($is_code_block) {
            push @code_block_buf, $_;
        } else {
            print doku_link($line)."\n";
        }

    } else {
        if (/^(\*+)\ (.*)/) {
            $head_indent = length($1);
            if (!($2 =~ /^!/)) {
                if ($head_indent <= $last_head_indent) {
                    $is_published = 1;
                    goto retry;
                }
            }
        }
    }
}
