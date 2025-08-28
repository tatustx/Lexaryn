;; Lexaryn - Collaborative Clarity Encyclopedia
;; A community-driven knowledge base for Clarity smart contract development
;; FIXED VERSION with safe data access

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_NOT_FOUND (err u2))
(define-constant ERR_ALREADY_EXISTS (err u3))
(define-constant ERR_INVALID_INPUT (err u4))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u5))
(define-constant ERR_CONTENT_TOO_LARGE (err u6))

;; Data Variables
(define-data-var next-snippet-id uint u1)
(define-data-var next-category-id uint u1)
(define-data-var min-reputation-to-moderate uint u100)

;; Data Maps
(define-map snippets
  { id: uint }
  {
    title: (string-ascii 200),
    description: (string-utf8 500),
    code: (string-utf8 2000),
    category-id: uint,
    author: principal,
    created-at: uint,
    updated-at: uint,
    votes: int,
    is-verified: bool
  }
)

(define-map categories
  { id: uint }
  {
    name: (string-ascii 100),
    description: (string-utf8 300),
    created-by: principal,
    snippet-count: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    username: (string-ascii 50),
    reputation: uint,
    snippets-contributed: uint,
    votes-cast: uint,
    is-moderator: bool
  }
)

(define-map snippet-votes
  { snippet-id: uint, voter: principal }
  { vote-type: bool } ;; true for upvote, false for downvote
)

(define-map moderators
  { moderator: principal }
  { approved-by: principal, approved-at: uint }
)

;; Read-only functions

(define-read-only (get-snippet (snippet-id uint))
  (map-get? snippets { id: snippet-id })
)

(define-read-only (get-category (category-id uint))
  (map-get? categories { id: category-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-next-snippet-id)
  (var-get next-snippet-id)
)

(define-read-only (get-next-category-id)
  (var-get next-category-id)
)

(define-read-only (has-user-voted (snippet-id uint) (voter principal))
  (let ((vote-key { snippet-id: snippet-id, voter: voter }))
    (is-some (map-get? snippet-votes vote-key))
  )
)

(define-read-only (is-moderator (user principal))
  (match (map-get? user-profiles { user: user })
    profile (get is-moderator profile)
    false
  )
)

(define-read-only (get-user-vote (snippet-id uint) (voter principal))
  (let ((vote-key { snippet-id: snippet-id, voter: voter }))
    (map-get? snippet-votes vote-key)
  )
)

;; Private functions

(define-private (update-user-reputation (user principal) (change int))
  (match (map-get? user-profiles { user: user })
    current-profile 
      (let ((current-rep (get reputation current-profile))
            (new-reputation (if (>= change 0)
                              (+ current-rep (to-uint change))
                              (if (>= current-rep (to-uint (- change)))
                                (- current-rep (to-uint (- change)))
                                u0))))
        (map-set user-profiles
          { user: user }
          {
            username: (get username current-profile),
            reputation: new-reputation,
            snippets-contributed: (get snippets-contributed current-profile),
            votes-cast: (get votes-cast current-profile),
            is-moderator: (get is-moderator current-profile)
          }
        )
        new-reputation
      )
    ;; If no profile exists, create one with the reputation change
    (let ((initial-reputation (if (>= change 0) (to-uint change) u0)))
      (map-set user-profiles
        { user: user }
        { username: "", reputation: initial-reputation, snippets-contributed: u0, votes-cast: u0, is-moderator: false }
      )
      initial-reputation
    )
  )
)

;; Public functions

(define-public (create-category (name (string-ascii 100)) (description (string-utf8 300)))
  (let ((category-id (var-get next-category-id))
        (name-len (len name))
        (desc-len (len description))
        (category-key { id: category-id }))
    (asserts! (> name-len u0) ERR_INVALID_INPUT)
    (asserts! (<= name-len u100) ERR_INVALID_INPUT)
    (asserts! (<= desc-len u300) ERR_INVALID_INPUT)
    
    (map-set categories
      category-key
      {
        name: name,
        description: description,
        created-by: tx-sender,
        snippet-count: u0
      }
    )
    
    (var-set next-category-id (+ category-id u1))
    (ok category-id)
  )
)

(define-public (create-snippet 
  (title (string-ascii 200))
  (description (string-utf8 500))
  (code (string-utf8 2000))
  (category-id uint))
  
  (let ((snippet-id (var-get next-snippet-id))
        (current-block stacks-block-height)
        (title-len (len title))
        (desc-len (len description))
        (code-len (len code))
        (snippet-key { id: snippet-id })
        (category-key { id: category-id }))
    
    ;; Validation
    (asserts! (> title-len u0) ERR_INVALID_INPUT)
    (asserts! (> code-len u0) ERR_INVALID_INPUT)
    (asserts! (<= title-len u200) ERR_INVALID_INPUT)
    (asserts! (<= desc-len u500) ERR_INVALID_INPUT)
    (asserts! (<= code-len u2000) ERR_INVALID_INPUT)
    (asserts! (is-some (get-category category-id)) ERR_NOT_FOUND)
    
    ;; Create snippet
    (map-set snippets
      snippet-key
      {
        title: title,
        description: description,
        code: code,
        category-id: category-id,
        author: tx-sender,
        created-at: current-block,
        updated-at: current-block,
        votes: 0,
        is-verified: false
      }
    )
    
    ;; Update category snippet count - FIXED with safe data access
    (match (get-category category-id)
      category (let ((cat-name (get name category))
                     (cat-desc (get description category))
                     (cat-creator (get created-by category))
                     (cat-count (get snippet-count category)))
                 (map-set categories
                   category-key
                   {
                     name: cat-name,
                     description: cat-desc,
                     created-by: cat-creator,
                     snippet-count: (+ cat-count u1)
                   })
                 true)
      false
    )
    
    ;; Update user profile
    (match (map-get? user-profiles { user: tx-sender })
      current-profile (begin
                        (map-set user-profiles
                          { user: tx-sender }
                          {
                            username: (get username current-profile),
                            reputation: (get reputation current-profile),
                            snippets-contributed: (+ (get snippets-contributed current-profile) u1),
                            votes-cast: (get votes-cast current-profile),
                            is-moderator: (get is-moderator current-profile)
                          })
                        true)
      (map-set user-profiles
        { user: tx-sender }
        { username: "", reputation: u10, snippets-contributed: u1, votes-cast: u0, is-moderator: false })
    )
    
    (var-set next-snippet-id (+ snippet-id u1))
    (update-user-reputation tx-sender 10)
    (ok snippet-id)
  )
)

(define-public (vote-snippet (snippet-id uint) (is-upvote bool))
  (let ((snippet-data (unwrap! (get-snippet snippet-id) ERR_NOT_FOUND))
        (vote-key { snippet-id: snippet-id, voter: tx-sender })
        (existing-vote (map-get? snippet-votes vote-key))
        (snippet-key { id: snippet-id }))
    
    ;; Check if user already voted
    (asserts! (is-none existing-vote) ERR_ALREADY_EXISTS)
    
    ;; Safe data extraction - fields are guaranteed to exist in the schema
    (let ((title (get title snippet-data))
          (description (get description snippet-data))
          (code (get code snippet-data))
          (category-id (get category-id snippet-data))
          (author (get author snippet-data))
          (created-at (get created-at snippet-data))
          (updated-at (get updated-at snippet-data))
          (votes (get votes snippet-data))
          (is-verified (get is-verified snippet-data)))
      
      ;; Cannot vote on own snippet
      (asserts! (not (is-eq tx-sender author)) ERR_UNAUTHORIZED)
      
      ;; Record the vote
      (map-set snippet-votes
        vote-key
        { vote-type: is-upvote }
      )
      
      ;; Validate extracted data
      (asserts! (> (len title) u0) ERR_INVALID_INPUT)
      (asserts! (<= (len title) u200) ERR_INVALID_INPUT)
      (asserts! (<= (len description) u500) ERR_INVALID_INPUT)
      (asserts! (<= (len code) u2000) ERR_INVALID_INPUT)
      
      ;; Update snippet vote count with validated data
      (map-set snippets
        snippet-key
        {
          title: title,
          description: description,
          code: code,
          category-id: category-id,
          author: author,
          created-at: created-at,
          updated-at: updated-at,
          votes: (+ votes (if is-upvote 1 -1)),
          is-verified: is-verified
        }
      )
      
      ;; Update voter's profile
      (match (map-get? user-profiles { user: tx-sender })
        current-profile (begin
                          (map-set user-profiles
                            { user: tx-sender }
                            {
                              username: (get username current-profile),
                              reputation: (get reputation current-profile),
                              snippets-contributed: (get snippets-contributed current-profile),
                              votes-cast: (+ (get votes-cast current-profile) u1),
                              is-moderator: (get is-moderator current-profile)
                            })
                          true)
        (map-set user-profiles
          { user: tx-sender }
          { username: "", reputation: u0, snippets-contributed: u0, votes-cast: u1, is-moderator: false })
      )
      
      ;; Safe reputation update
      (update-user-reputation author (if is-upvote 5 -2))
      (ok true)
    )
  )
)

(define-public (update-snippet 
  (snippet-id uint)
  (title (string-ascii 200))
  (description (string-utf8 500))
  (code (string-utf8 2000)))
  
  (let ((snippet-data (unwrap! (get-snippet snippet-id) ERR_NOT_FOUND))
        (title-len (len title))
        (desc-len (len description))
        (code-len (len code))
        (snippet-key { id: snippet-id }))
    
    ;; Safe extraction - fields are guaranteed by schema
    (let ((author (get author snippet-data))
          (category-id (get category-id snippet-data))
          (created-at (get created-at snippet-data))
          (votes (get votes snippet-data))
          (is-verified (get is-verified snippet-data)))
      
      ;; Only author can update
      (asserts! (is-eq tx-sender author) ERR_UNAUTHORIZED)
      
      ;; Validation
      (asserts! (> title-len u0) ERR_INVALID_INPUT)
      (asserts! (> code-len u0) ERR_INVALID_INPUT)
      (asserts! (<= title-len u200) ERR_INVALID_INPUT)
      (asserts! (<= desc-len u500) ERR_INVALID_INPUT)
      (asserts! (<= code-len u2000) ERR_INVALID_INPUT)
      
      ;; Validate extracted data before using
      (asserts! (> (len title) u0) ERR_INVALID_INPUT)
      (asserts! (<= (len title) u200) ERR_INVALID_INPUT)
      (asserts! (<= (len description) u500) ERR_INVALID_INPUT)
      (asserts! (<= (len code) u2000) ERR_INVALID_INPUT)
      
      (map-set snippets
        snippet-key
        {
          title: title,
          description: description,
          code: code,
          category-id: category-id,
          author: author,
          created-at: created-at,
          updated-at: stacks-block-height,
          votes: votes,
          is-verified: is-verified
        }
      )
      
      (ok true)
    )
  )
)

(define-public (verify-snippet (snippet-id uint))
  (let ((snippet-data (unwrap! (get-snippet snippet-id) ERR_NOT_FOUND))
        (snippet-key { id: snippet-id }))
    
    ;; Only moderators can verify
    (asserts! (is-moderator tx-sender) ERR_UNAUTHORIZED)
    
    ;; Fields are guaranteed by map schema - no unwrap needed
    (let ((title (get title snippet-data))
          (description (get description snippet-data))
          (code (get code snippet-data))
          (category-id (get category-id snippet-data))
          (author (get author snippet-data))
          (created-at (get created-at snippet-data))
          (updated-at (get updated-at snippet-data))
          (votes (get votes snippet-data)))
      
      ;; Validate extracted data before using
      (asserts! (> (len title) u0) ERR_INVALID_INPUT)
      (asserts! (<= (len title) u200) ERR_INVALID_INPUT)
      (asserts! (<= (len description) u500) ERR_INVALID_INPUT)
      (asserts! (<= (len code) u2000) ERR_INVALID_INPUT)
      
      (map-set snippets
        snippet-key
        {
          title: title,
          description: description,
          code: code,
          category-id: category-id,
          author: author,
          created-at: created-at,
          updated-at: updated-at,
          votes: votes,
          is-verified: true
        }
      )
      
      ;; Give bonus reputation to author - now using safe author variable
      (update-user-reputation author 25)
      (ok true)
    )
  )
)

(define-public (set-username (username (string-ascii 50)))
  (let ((username-len (len username)))
    (asserts! (> username-len u0) ERR_INVALID_INPUT)
    (asserts! (<= username-len u50) ERR_INVALID_INPUT)
    
    (match (map-get? user-profiles { user: tx-sender })
      current-profile (begin
                        (map-set user-profiles
                          { user: tx-sender }
                          {
                            username: username,
                            reputation: (get reputation current-profile),
                            snippets-contributed: (get snippets-contributed current-profile),
                            votes-cast: (get votes-cast current-profile),
                            is-moderator: (get is-moderator current-profile)
                          })
                        (ok true))
      (begin
        (map-set user-profiles
          { user: tx-sender }
          { username: username, reputation: u0, snippets-contributed: u0, votes-cast: u0, is-moderator: false })
        (ok true))
    )
  )
)

(define-public (add-moderator (new-moderator principal))
  (let ((moderator-key { moderator: new-moderator })
        (user-key { user: new-moderator }))
    ;; Only contract owner can add moderators
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set moderators
      moderator-key
      { approved-by: tx-sender, approved-at: stacks-block-height }
    )
    
    ;; Update user profile to mark as moderator
    (match (map-get? user-profiles user-key)
      current-profile (begin
                        (map-set user-profiles
                          user-key
                          {
                            username: (get username current-profile),
                            reputation: (get reputation current-profile),
                            snippets-contributed: (get snippets-contributed current-profile),
                            votes-cast: (get votes-cast current-profile),
                            is-moderator: true
                          })
                        true)
      (map-set user-profiles
        user-key
        { username: "", reputation: u0, snippets-contributed: u0, votes-cast: u0, is-moderator: true })
    )
    
    (ok true)
  )
)

(define-public (remove-moderator (moderator principal))
  (let ((moderator-key { moderator: moderator })
        (user-key { user: moderator }))
    ;; Only contract owner can remove moderators
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-delete moderators moderator-key)
    
    ;; Update user profile
    (match (map-get? user-profiles user-key)
      current-profile (begin
                        (map-set user-profiles
                          user-key
                          {
                            username: (get username current-profile),
                            reputation: (get reputation current-profile),
                            snippets-contributed: (get snippets-contributed current-profile),
                            votes-cast: (get votes-cast current-profile),
                            is-moderator: false
                          })
                        true)
      true ;; If no profile exists, no need to update
    )
    
    (ok true)
  )
)