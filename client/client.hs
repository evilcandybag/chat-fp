
import Crypto.Scrypt 
import Data.Map

data Avatar = ImageB64 ByteString
            | GravatarMail String
            | ImageURL URI
--            | Server stored/encrypted binary image? 
            | None

newtype User
  = User (PrivKey, PubKey) Avatar Name

-- Locally generate a Public Key pair
generateKeyPair :: RandomGen g =>
                   g -> Salt -> (PrivKey, PubKey)
generateKeyPair = undefined

-- Encrypt a message using a public key
encrypt :: Text -> PubKey -> Text
encrypt = undefined

-- Decrypt a message using a private key
decrypt :: Text -> PrivKey -> Text
decrypt = undefined

