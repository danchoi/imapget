require 'net/imap'




xs = ["&AOkA6QDp-", "&bElbVw-", "&bElbVw-/&byJbVw-"]
xs.each do |s|
  puts (Net::IMAP.decode_utf7 s)
  puts s
  re = /&(.*?)-/n
  re = /&([^-]*)-/n
  puts s.gsub(re) {
          puts $1
          base64 = $1.tr(",", "/")
          x = base64.length % 4
          if x > 0
            base64.concat("=" * (4 - x))
          end
          #puts base64
          #puts base64.unpack("m").inspect
          #puts base64.unpack("m")[0].inspect
          puts base64.unpack("m")[0].unpack("n*").inspect
          puts 'test'
          base64.unpack("m")[0].unpack("n*").pack("U*")
  }

  puts '---'
end
